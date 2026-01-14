#pragma once
#include "MapperBase.h"
#include <vector>
#include <cstring>

namespace nes {

    class Mapper004 : public MapperBase {
    private:
        // Internal Registers
        uint8_t _registers[8]{ 0 };
        uint32_t _prgBanksReg[4]{ 0 };
        uint32_t _chrBanksReg[8]{ 0 };

        // Control flags
        uint8_t _targetRegister = 0;
        bool _prgBankMode = false;
        bool _chrInversion = false;

        // IRQ Counter Logic
        uint16_t _irqCounter = 0;
        uint16_t _irqReload = 0;
        bool _irqEnable = false;
        bool _irqActive = false;

        void UpdateBanks() {
            // Update CHR banks (8 x 1KB banks)
            if (_chrInversion) {
                _chrBanksReg[0] = static_cast<uint32_t>(_registers[2]) * 0x0400;
                _chrBanksReg[1] = static_cast<uint32_t>(_registers[3]) * 0x0400;
                _chrBanksReg[2] = static_cast<uint32_t>(_registers[4]) * 0x0400;
                _chrBanksReg[3] = static_cast<uint32_t>(_registers[5]) * 0x0400;
                _chrBanksReg[4] = static_cast<uint32_t>(_registers[0] & 0xFE) * 0x0400;
                _chrBanksReg[5] = static_cast<uint32_t>(_registers[0] | 0x01) * 0x0400;
                _chrBanksReg[6] = static_cast<uint32_t>(_registers[1] & 0xFE) * 0x0400;
                _chrBanksReg[7] = static_cast<uint32_t>(_registers[1] | 0x01) * 0x0400;
            }
            else {
                _chrBanksReg[0] = static_cast<uint32_t>(_registers[0] & 0xFE) * 0x0400;
                _chrBanksReg[1] = static_cast<uint32_t>(_registers[0] | 0x01) * 0x0400;
                _chrBanksReg[2] = static_cast<uint32_t>(_registers[1] & 0xFE) * 0x0400;
                _chrBanksReg[3] = static_cast<uint32_t>(_registers[1] | 0x01) * 0x0400;
                _chrBanksReg[4] = static_cast<uint32_t>(_registers[2]) * 0x0400;
                _chrBanksReg[5] = static_cast<uint32_t>(_registers[3]) * 0x0400;
                _chrBanksReg[6] = static_cast<uint32_t>(_registers[4]) * 0x0400;
                _chrBanksReg[7] = static_cast<uint32_t>(_registers[5]) * 0x0400;
            }

            // Update PRG banks (4 x 8KB banks)
            // MMC3 uses 8KB banks, so we use _prgBanks * 2 to represent 8KB units
            uint32_t lastBank = (_prgBanks * 2) - 1;
            uint32_t secondLastBank = (_prgBanks * 2) - 2;

            if (_prgBankMode) {
                _prgBanksReg[0] = secondLastBank * 0x2000;
                _prgBanksReg[1] = static_cast<uint32_t>(_registers[7] & 0x3F) * 0x2000;
                _prgBanksReg[2] = static_cast<uint32_t>(_registers[6] & 0x3F) * 0x2000;
                _prgBanksReg[3] = lastBank * 0x2000;
            }
            else {
                _prgBanksReg[0] = static_cast<uint32_t>(_registers[6] & 0x3F) * 0x2000;
                _prgBanksReg[1] = static_cast<uint32_t>(_registers[7] & 0x3F) * 0x2000;
                _prgBanksReg[2] = secondLastBank * 0x2000;
                _prgBanksReg[3] = lastBank * 0x2000;
            }
        }

    public:
        Mapper004(uint8_t prgBanks, uint8_t chrBanks) : MapperBase(prgBanks, chrBanks) {
            _cartRam.assign(8192, 0);
            Reset();
        }

        uint8_t GetMapperNumber() const override { return 4; }
        std::string GetMapperName() const override { return "MMC3 (TxROM)"; }

        void Reset() override {
            _targetRegister = 0;
            _prgBankMode = false;
            _chrInversion = false;
            _mirrorMode = MirrorMode::Horizontal;

            _irqCounter = 0;
            _irqReload = 0;
            _irqEnable = false;
            _irqActive = false;

            std::memset(_registers, 0, sizeof(_registers));
            std::memset(_prgBanksReg, 0, sizeof(_prgBanksReg));
            std::memset(_chrBanksReg, 0, sizeof(_chrBanksReg));

            // Initial PRG bank setup
            _prgBanksReg[0] = 0;
            _prgBanksReg[1] = 0x2000;
            _prgBanksReg[2] = ((_prgBanks * 2) - 2) * 0x2000;
            _prgBanksReg[3] = ((_prgBanks * 2) - 1) * 0x2000;
        }

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                mappedAddr = 0xFFFFFFFF;
                data = _cartRam[addr & 0x1FFF];
                return true;
            }

            if (addr >= 0x8000) {
                int index = (addr - 0x8000) / 0x2000;
                mappedAddr = _prgBanksReg[index] + (addr & 0x1FFF);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                mappedAddr = 0xFFFFFFFF;
                _cartRam[addr & 0x1FFF] = data;
                return true;
            }

            if (addr >= 0x8000 && addr <= 0x9FFF) {
                if (!(addr & 1)) {
                    _targetRegister = data & 0x07;
                    _prgBankMode = (data & 0x40) != 0;
                    _chrInversion = (data & 0x80) != 0;
                }
                else {
                    _registers[_targetRegister] = data;
                    UpdateBanks();
                }
            }
            else if (addr >= 0xA000 && addr <= 0xBFFF) {
                if (!(addr & 1)) {
                    _mirrorMode = (data & 1) ? MirrorMode::Horizontal : MirrorMode::Vertical;
                }
            }
            else if (addr >= 0xC000 && addr <= 0xDFFF) {
                if (!(addr & 1)) {
                    _irqReload = data;
                }
                else {
                    _irqCounter = 0;
                }
            }
            else if (addr >= 0xE000) {
                if (!(addr & 1)) {
                    _irqEnable = false;
                    _irqActive = false;
                }
                else {
                    _irqEnable = true;
                }
            }
            return false;
        }

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                uint8_t bankIndex = addr / 0x0400;
                mappedAddr = _chrBanksReg[bankIndex] + (addr & 0x03FF);
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            return false; // MMC3 typically uses CHR-ROM
        }

        bool IsIrqActive() const override { return _irqActive; }
        void ClearIrq() override { _irqActive = false; }

        void ScanlineCounter() override {
            if (_irqCounter == 0) {
                _irqCounter = _irqReload;
            }
            else {
                _irqCounter--;
            }

            if (_irqCounter == 0 && _irqEnable) {
                _irqActive = true;
            }
        }

        MirrorMode GetMirrorMode() const override { return _mirrorMode; }
    };
}
