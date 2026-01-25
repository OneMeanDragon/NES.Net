#pragma once
#include "MapperBase.h"
#include <vector>
#include <cstring>

namespace nes {

    class Mapper004 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 4;
        static constexpr const char* NAME = "MMC3 (TxROM)";
        static constexpr const char* INFO = "Nintendo MMC3. Advanced banking, scanline IRQ counter, 8KB cart RAM.";
        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

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
        uint8_t _irqCounter = 0;
        uint8_t _irqReload = 0;
        bool _irqEnable = false;
        bool _irqActive = false;
        bool _irqReloadFlag = false;

        // PRG RAM control
        bool _cartRamEnabled = true;
        bool _cartRamWriteProtected = false;

        // A12 tracking for accurate IRQ
        bool _lastA12 = false;
        uint16_t _a12LowCount = 0;  // Count how long A12 has been low

        void UpdateBanks() {
            // Update CHR banks
            // Registers 0-1: 2KB banks (mask bit 0 for even alignment)
            // Registers 2-5: 1KB banks
            if (_chrInversion) {
                // Inverted: 2KB banks at $1000-$17FF, 1KB banks at $0000-$0FFF
                _chrBanksReg[4] = static_cast<uint32_t>(_registers[0] & 0xFE) * 0x0400;
                _chrBanksReg[5] = _chrBanksReg[4] + 0x0400;
                _chrBanksReg[6] = static_cast<uint32_t>(_registers[1] & 0xFE) * 0x0400;
                _chrBanksReg[7] = _chrBanksReg[6] + 0x0400;

                _chrBanksReg[0] = static_cast<uint32_t>(_registers[2]) * 0x0400;
                _chrBanksReg[1] = static_cast<uint32_t>(_registers[3]) * 0x0400;
                _chrBanksReg[2] = static_cast<uint32_t>(_registers[4]) * 0x0400;
                _chrBanksReg[3] = static_cast<uint32_t>(_registers[5]) * 0x0400;
            }
            else {
                // Normal: 2KB banks at $0000-$0FFF, 1KB banks at $1000-$17FF
                _chrBanksReg[0] = static_cast<uint32_t>(_registers[0] & 0xFE) * 0x0400;
                _chrBanksReg[1] = _chrBanksReg[0] + 0x0400;
                _chrBanksReg[2] = static_cast<uint32_t>(_registers[1] & 0xFE) * 0x0400;
                _chrBanksReg[3] = _chrBanksReg[2] + 0x0400;

                _chrBanksReg[4] = static_cast<uint32_t>(_registers[2]) * 0x0400;
                _chrBanksReg[5] = static_cast<uint32_t>(_registers[3]) * 0x0400;
                _chrBanksReg[6] = static_cast<uint32_t>(_registers[4]) * 0x0400;
                _chrBanksReg[7] = static_cast<uint32_t>(_registers[5]) * 0x0400;
            }

            // Update PRG banks - CORRECT MMC3 behavior
            // _prgBanks is in 16KB units, convert to 8KB units
            uint32_t prg8KBanks = _prgBanks * 2;
            uint32_t lastBank = (prg8KBanks > 0) ? (prg8KBanks - 1) : 0;
            uint32_t secondLastBank = (prg8KBanks > 1) ? (prg8KBanks - 2) : 0;

            if (_prgBankMode) {
                // Mode 1 (bit 6 set): Fixed at $8000, R7 at $A000, R6 at $C000, Fixed at $E000
                _prgBanksReg[0] = secondLastBank * 0x2000;
                _prgBanksReg[1] = static_cast<uint32_t>(_registers[7] & 0x3F) * 0x2000;
                _prgBanksReg[2] = static_cast<uint32_t>(_registers[6] & 0x3F) * 0x2000;
                _prgBanksReg[3] = lastBank * 0x2000;
            }
            else {
                // Mode 0 (bit 6 clear): R6 at $8000, R7 at $A000, Fixed at $C000, Fixed at $E000
                _prgBanksReg[0] = static_cast<uint32_t>(_registers[6] & 0x3F) * 0x2000;
                _prgBanksReg[1] = static_cast<uint32_t>(_registers[7] & 0x3F) * 0x2000;
                _prgBanksReg[2] = secondLastBank * 0x2000;
                _prgBanksReg[3] = lastBank * 0x2000;
            }

            // Wrap banks that exceed ROM size
            uint32_t prgRomSize = prg8KBanks * 0x2000;
            if (prgRomSize > 0) {
                for (int i = 0; i < 4; i++) {
                    _prgBanksReg[i] %= prgRomSize;
                }
            }

            if (_chrBanks > 0) {  // CHR-ROM
                uint32_t chrRomSize = _chrBanks * 0x2000;
                for (int i = 0; i < 8; i++) {
                    _chrBanksReg[i] %= chrRomSize;
                }
            }
        }

    public:
        Mapper004(uint8_t prgBanks, uint8_t chrBanks) : MapperBase(prgBanks, chrBanks) {
            _cartRam.assign(8192, 0);
            Reset();
        }

        void Reset() override {
            _targetRegister = 0;
            _prgBankMode = false;
            _chrInversion = false;

            // Reset mirroring back to cartridge header default
            _mirrorMode = _initialMirrorMode;

            _irqCounter = 0;
            _irqReload = 0;
            _irqEnable = false;
            _irqActive = false;
            _irqReloadFlag = false;

            _cartRamEnabled = true;
            _cartRamWriteProtected = false;
            _lastA12 = false;
            _a12LowCount = 0;

            std::memset(_registers, 0, sizeof(_registers));

            // Initialize registers with sensible defaults
            _registers[6] = 0;  // PRG bank 0
            _registers[7] = 1;  // PRG bank 1

            // Set up initial banks (power-on state)
            UpdateBanks();
        }

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                if (!_cartRamEnabled) return false;
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
                if (!_cartRamEnabled || _cartRamWriteProtected) return false;
                mappedAddr = 0xFFFFFFFF;
                _cartRam[addr & 0x1FFF] = data;
                return true;
            }

            if (addr >= 0x8000 && addr <= 0x9FFF) {
                if (addr & 1) {
                    // Bank data ($8001, $8003, etc.)
                    _registers[_targetRegister] = data;
                    UpdateBanks();
                }
                else {
                    // Bank select ($8000, $8002, etc.)
                    _targetRegister = data & 0x07;
                    _prgBankMode = (data & 0x40) != 0;
                    _chrInversion = (data & 0x80) != 0;
                    UpdateBanks();
                }
            }
            else if (addr >= 0xA000 && addr <= 0xBFFF) {
                if (addr & 1) {
                    // PRG RAM protect ($A001, $A003, etc.)
                    _cartRamEnabled = (data & 0x80) != 0;
                    _cartRamWriteProtected = (data & 0x40) == 0;
                }
                else {
                    // Mirroring ($A000, $A002, etc.)
                    // Bit 0: 0 = vertical, 1 = horizontal
                    _mirrorMode = (data & 1) ? MirrorMode::Horizontal : MirrorMode::Vertical;
                }
            }
            else if (addr >= 0xC000 && addr <= 0xDFFF) {
                if (addr & 1) {
                    // IRQ reload ($C001, $C003, etc.)
                    // Writing here sets the reload flag - counter reloads on NEXT clock
                    _irqReloadFlag = true;
                }
                else {
                    // IRQ latch ($C000, $C002, etc.)
                    _irqReload = data;
                }
            }
            else if (addr >= 0xE000) {
                if (addr & 1) {
                    // IRQ enable ($E001, $E003, etc.)
                    _irqEnable = true;
                }
                else {
                    // IRQ disable ($E000, $E002, etc.)
                    _irqEnable = false;
                    _irqActive = false;  // Acknowledge/clear any pending IRQ
                }
            }
            return false;
        }

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                if (_chrBanks == 0) {
                    // CHR-RAM - direct mapping
                    mappedAddr = addr;
                }
                else {
                    // CHR-ROM - banked
                    uint8_t bankIndex = addr / 0x0400;
                    mappedAddr = _chrBanksReg[bankIndex] + (addr & 0x03FF);
                }
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // CHR-RAM support
                mappedAddr = addr;
                return true;
            }
            return false;
        }

        bool IsIrqActive() const override { return _irqActive; }
        void ClearIrq() override { _irqActive = false; }

        // Scanline counter - call once per scanline (scanlines 0-239, cycle 260)
        void ScanlineCounter() override {
            // Handle reload first
            if (_irqCounter == 0 || _irqReloadFlag) {
                _irqCounter = _irqReload;
                _irqReloadFlag = false;
            }
            else {
                // Normal decrement
                _irqCounter--;
            }

            // Check for IRQ after decrement/reload
            if (_irqCounter == 0 && _irqEnable) {
                _irqActive = true;
            }
        }

        MirrorMode GetMirrorMode() const override { return _mirrorMode; }
    };

} // namespace nes