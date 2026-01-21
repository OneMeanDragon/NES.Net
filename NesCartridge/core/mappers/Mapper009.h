#pragma once
#include "MapperBase.h"
#include <vector>

namespace nes {

    class Mapper009 : public MapperBase {
    public:
        static constexpr uint8_t     ID   = 9;
        static constexpr const char* NAME = "MMC2 (PxROM)";
        static constexpr const char* INFO = "Nintendo MMC2. 16KB PRG banking with special CHR banking for split-screen effects.";
        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }
    private:
        // PRG Reg: $A000-$AFFF
        int _prgBankSelect = 0;
        uint32_t _prgFixedOffset{ 0 };

        // CHR Regs: $B000-$EFFF (Note: These store 4KB bank indexes)
        int _chrBank0FD = 0; // Config for $0000-$0FFF range when Latch 0 is FD
        int _chrBank0FE = 0; // Config for $0000-$0FFF range when Latch 0 is FE
        int _chrBank1FD = 0; // Config for $1000-$1FFF range when Latch 1 is FD
        int _chrBank1FE = 0; // Config for $1000-$1FFF range when Latch 1 is FE

        // Latches (0 = FD state, 1 = FE state)
        int _latch0 = 0;
        int _latch1 = 0;

    public:
        Mapper009(uint8_t prgBanks16KB, uint8_t chrBanks8KB)
            // MMC2 uses 8KB PRG banks internally. 
            // The base class needs total number of banks in its size unit.
            : MapperBase(prgBanks16KB * 2, chrBanks8KB * 2) {

            // _prgBanks now holds total 8KB banks.
            int total8kbBanks = _prgBanks;

            // The fixed 24KB block (3 banks * 8KB) starts from this offset:
            _prgFixedOffset = static_cast<uint32_t>((total8kbBanks - 3) * 0x2000);

            _cartRam.assign(8192, 0); // Allocate 8KB cart RAM
            Reset();
        }

        void Reset() override {
            _prgBankSelect = 0;
            _chrBank0FD = 0;
            _chrBank0FE = 0;
            _chrBank1FD = 0;
            _chrBank1FE = 0;
            _latch0 = 0;
            _latch1 = 0;
            _mirrorMode = MirrorMode::Horizontal;
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            // PRG RAM: $6000-$7FFF (PlayChoice version only)
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                mappedAddr = 0xFFFFFFFF; // Sentinel for internal RAM
                data = _cartRam[addr & 0x1FFF];
                return true;
            }
            // Switchable 8KB bank at $8000-$9FFF
            else if (addr >= 0x8000 && addr <= 0x9FFF) {
                mappedAddr = static_cast<uint32_t>(_prgBankSelect * 0x2000) + (addr & 0x1FFF);
                return true;
            }
            // Fixed 24KB PRG ROM at $A000-$FFFF
            else if (addr >= 0xA000) {
                mappedAddr = _prgFixedOffset + (addr - 0xA000);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            // Cart RAM write
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                mappedAddr = 0xFFFFFFFF;
                _cartRam[addr & 0x1FFF] = data;
                return true;
            }

            // Mapper Register Writes
            if (addr >= 0xA000 && addr <= 0xAFFF) {
                _prgBankSelect = data & 0x0F;
            }
            else if (addr >= 0xB000 && addr <= 0xBFFF) {
                _chrBank0FD = data & 0x1F;
            }
            else if (addr >= 0xC000 && addr <= 0xCFFF) {
                _chrBank0FE = data & 0x1F;
            }
            else if (addr >= 0xD000 && addr <= 0xDFFF) {
                _chrBank1FD = data & 0x1F;
            }
            else if (addr >= 0xE000 && addr <= 0xEFFF) {
                _chrBank1FE = data & 0x1F;
            }
            else if (addr >= 0xF000 && addr <= 0xFFFF) {
                // Mirroring control (0 = Vertical, 1 = Horizontal)
                _mirrorMode = (data & 0x01) == 0 ? MirrorMode::Vertical : MirrorMode::Horizontal;
            }
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                // MMC2 maps 4KB banks, so we check the high bit of the address for which region we are in
                if (addr <= 0x0FFF) { // $0000-$0FFF range
                    // Select bank based on Latch 0 state
                    int bank = (_latch0 == 0) ? _chrBank0FD : _chrBank0FE;
                    mappedAddr = static_cast<uint32_t>(bank * 0x1000) + (addr & 0x0FFF);
                }
                else { // $1000-$1FFF range
                    // Select bank based on Latch 1 state
                    int bank = (_latch1 == 0) ? _chrBank1FD : _chrBank1FE;
                    mappedAddr = static_cast<uint32_t>(bank * 0x1000) + (addr & 0x0FFF);
                }

                // CRITICAL: Update latches based on specific PPU address reads
                // Latch 0 (left pattern table) only triggers on single addresses
                if (addr == 0x0FD8) {
                    _latch0 = 0; // Set to FD state
                }
                else if (addr == 0x0FE8) {
                    _latch0 = 1; // Set to FE state
                }

                // Latch 1 (right pattern table) triggers on ranges
                if (addr >= 0x1FD8 && addr <= 0x1FDF) {
                    _latch1 = 0; // Set to FD state
                }
                else if (addr >= 0x1FE8 && addr <= 0x1FEF) {
                    _latch1 = 1; // Set to FE state
                }

                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr <= 0x1FFF && _chrBanks == 0) {
                // Handle the rare case where CHR-RAM is used with MMC2 logic
                if (addr <= 0x0FFF) {
                    int bank = (_latch0 == 0) ? _chrBank0FD : _chrBank0FE;
                    mappedAddr = static_cast<uint32_t>(bank * 0x1000) + (addr & 0x0FFF);
                }
                else {
                    int bank = (_latch1 == 0) ? _chrBank1FD : _chrBank1FE;
                    mappedAddr = static_cast<uint32_t>(bank * 0x1000) + (addr & 0x0FFF);
                }

                // Update latches on writes too (though rare with CHR-RAM)
                if (addr == 0x0FD8) {
                    _latch0 = 0;
                }
                else if (addr == 0x0FE8) {
                    _latch0 = 1;
                }

                if (addr >= 0x1FD8 && addr <= 0x1FDF) {
                    _latch1 = 0;
                }
                else if (addr >= 0x1FE8 && addr <= 0x1FEF) {
                    _latch1 = 1;
                }

                return true;
            }
            return false; // Typically CHR-ROM
        }
    };
}