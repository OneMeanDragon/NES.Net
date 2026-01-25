#pragma once
#include "MapperBase.h"
#include <cstring>

namespace nes {

    class Mapper004 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 4;
        static constexpr const char* NAME = "MMC3 (TxROM)";
        static constexpr const char* INFO = "Nintendo MMC3. PRG/CHR banking with scanline IRQ.";

        constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        constexpr const char* GetMapperName() const noexcept override { return NAME; }
        constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

    private:
        // ============================================================
        // BANK REGISTERS
        // ============================================================

        uint8_t  _bankRegs[8]{};
        uint32_t _prgMap[4]{};
        uint32_t _chrMap[8]{};

        uint8_t _bankSelect = 0;
        bool    _prgMode = false;
        bool    _chrInvert = false;

        // ============================================================
        // IRQ STATE
        // ============================================================

        uint8_t _irqCounter = 0;
        uint8_t _irqReloadValue = 0;
        bool    _irqEnable = false;
        bool    _irqActive = false;
        bool    _irqReloadNext = false;

        // ============================================================
        // PRG RAM CONTROL
        // ============================================================

        bool _ramEnable = true;
        bool _ramProtect = false;

        // ============================================================
        // INTERNAL
        // ============================================================

        void UpdateBanks() {
            // ----------------------------
            // CHR BANKING (1KB units)
            // ----------------------------

            if (_chrInvert) {
                // 2KB banks at $1000, 1KB banks at $0000
                _chrMap[4] = (_bankRegs[0] & 0xFE) * 0x0400;
                _chrMap[5] = _chrMap[4] + 0x0400;
                _chrMap[6] = (_bankRegs[1] & 0xFE) * 0x0400;
                _chrMap[7] = _chrMap[6] + 0x0400;

                for (int i = 0; i < 4; i++)
                    _chrMap[i] = _bankRegs[i + 2] * 0x0400;
            }
            else {
                // 2KB banks at $0000, 1KB banks at $1000
                _chrMap[0] = (_bankRegs[0] & 0xFE) * 0x0400;
                _chrMap[1] = _chrMap[0] + 0x0400;
                _chrMap[2] = (_bankRegs[1] & 0xFE) * 0x0400;
                _chrMap[3] = _chrMap[2] + 0x0400;

                for (int i = 0; i < 4; i++)
                    _chrMap[i + 4] = _bankRegs[i + 2] * 0x0400;
            }

            // ----------------------------
            // PRG BANKING (8KB units)
            // ----------------------------

            uint32_t prg8kBanks = _prgBanks * 2;
            uint32_t last = (prg8kBanks > 0) ? prg8kBanks - 1 : 0;
            uint32_t secondLast = (prg8kBanks > 1) ? prg8kBanks - 2 : 0;

            if (_prgMode) {
                _prgMap[0] = secondLast * 0x2000;
                _prgMap[1] = (_bankRegs[7] & 0x3F) * 0x2000;
                _prgMap[2] = (_bankRegs[6] & 0x3F) * 0x2000;
                _prgMap[3] = last * 0x2000;
            }
            else {
                _prgMap[0] = (_bankRegs[6] & 0x3F) * 0x2000;
                _prgMap[1] = (_bankRegs[7] & 0x3F) * 0x2000;
                _prgMap[2] = secondLast * 0x2000;
                _prgMap[3] = last * 0x2000;
            }

            // Emulator-side safety wrapping
            uint32_t prgSize = prg8kBanks * 0x2000;
            if (prgSize) {
                for (auto& b : _prgMap)
                    b %= prgSize;
            }

            if (_chrBanks) {
                uint32_t chrSize = _chrBanks * 0x2000;
                for (auto& b : _chrMap)
                    b %= chrSize;
            }
        }

    public:
        Mapper004(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks)
        {
            _cartRam.assign(8192, 0);
            Reset();
        }

        void Reset() override {
            std::memset(_bankRegs, 0, sizeof(_bankRegs));

            _bankSelect = 0;
            _prgMode = false;
            _chrInvert = false;

            _irqCounter = 0;
            _irqReloadValue = 0;
            _irqEnable = false;
            _irqActive = false;
            _irqReloadNext = false;

            _ramEnable = true;
            _ramProtect = false;

            _mirrorMode = _initialMirrorMode;

            _bankRegs[6] = 0;
            _bankRegs[7] = 1;

            UpdateBanks();
        }

        // ============================================================
        // CPU MAP
        // ============================================================

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                if (!_ramEnable) return false;
                mappedAddr = CARTRAM_SIGNAL;
                data = _cartRam[addr & 0x1FFF];
                return true;
            }

            if (addr >= 0x8000) {
                uint8_t slot = (addr >> 13) & 0x03;
                mappedAddr = _prgMap[slot] + (addr & 0x1FFF);
                return true;
            }

            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                if (!_ramEnable || _ramProtect) return false;
                mappedAddr = CARTRAM_SIGNAL;
                _cartRam[addr & 0x1FFF] = data;
                return true;
            }

            if (addr >= 0x8000 && addr <= 0x9FFF) {
                if (addr & 1) {
                    _bankRegs[_bankSelect] = data;
                    UpdateBanks();
                }
                else {
                    _bankSelect = data & 0x07;
                    _prgMode = data & 0x40;
                    _chrInvert = data & 0x80;
                    UpdateBanks();
                }
            }
            else if (addr >= 0xA000 && addr <= 0xBFFF) {
                if (addr & 1) {
                    _ramEnable = data & 0x80;
                    _ramProtect = !(data & 0x40);
                }
                else {
                    _mirrorMode = (data & 1) ? MirrorMode::Horizontal
                        : MirrorMode::Vertical;
                }
            }
            else if (addr >= 0xC000 && addr <= 0xDFFF) {
                if (addr & 1) {
                    _irqReloadNext = true;
                }
                else {
                    _irqReloadValue = data;
                }
            }
            else if (addr >= 0xE000) {
                if (addr & 1) {
                    _irqEnable = true;
                }
                else {
                    _irqEnable = false;
                    _irqActive = false;
                }
            }

            return false;
        }

        // ============================================================
        // PPU MAP
        // ============================================================

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                if (_chrBanks == 0) {
                    mappedAddr = addr;
                }
                else {
                    uint8_t bank = addr >> 10;
                    mappedAddr = _chrMap[bank] + (addr & 0x03FF);
                }
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                mappedAddr = addr;
                return true;
            }
            return false;
        }

        // ============================================================
        // IRQ
        // ============================================================

        bool IsIrqActive() const override { return _irqActive; }
        void ClearIrq() override { _irqActive = false; }

        void ScanlineCounter() override {
            if (_irqCounter == 0 || _irqReloadNext) {
                _irqCounter = _irqReloadValue;
                _irqReloadNext = false;
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

} // namespace nes
