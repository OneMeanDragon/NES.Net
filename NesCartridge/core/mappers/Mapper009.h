#pragma once
#include "MapperBase.h"
#include "../Cartridge.h"

namespace nes {

    class Mapper009 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 9;
        static constexpr const char* NAME = "MMC2 (PxROM)";
        static constexpr const char* INFO = "Nintendo MMC2. 8KB PRG banking with special CHR latching for split-screen effects.";

        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

    private:
        // PRG banking
        uint8_t _prgBankSelect = 0;
        uint32_t _prgFixedOffset = 0;

        // CHR banking (4KB banks)
        uint8_t _chrBank0FD = 0; // $0000-$0FFF when latch 0 is FD
        uint8_t _chrBank0FE = 0; // $0000-$0FFF when latch 0 is FE
        uint8_t _chrBank1FD = 0; // $1000-$1FFF when latch 1 is FD
        uint8_t _chrBank1FE = 0; // $1000-$1FFF when latch 1 is FE

        // Latches (0 = FD state, 1 = FE state)
        uint8_t _latch0 = 0;
        uint8_t _latch1 = 0;

    public:
        Mapper009(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks)
        {
            // MMC2 uses 8KB PRG banks
            // The fixed 24KB block (3 banks * 8KB) starts from this offset
            uint32_t total8kbBanks = prgBanks * 2; // Convert 16KB units to 8KB units
            _prgFixedOffset = (total8kbBanks - 3) * 0x2000;

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

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            // PRG RAM: $6000-$7FFF (8KB)
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                region = MemoryRegion::PrgRam;
                mappedAddr = addr & 0x1FFF;
                return true;
            }
            // Switchable 8KB bank at $8000-$9FFF
            else if (addr >= 0x8000 && addr <= 0x9FFF) {
                region = MemoryRegion::PrgRom;
                // Mask to available 8KB banks
                uint32_t total8kbBanks = _prgBanks * 2;
                uint8_t bankMask = (total8kbBanks > 0) ? (total8kbBanks - 1) : 0x0F;
                mappedAddr = static_cast<uint32_t>(_prgBankSelect & bankMask) * 0x2000 + (addr & 0x1FFF);
                return true;
            }
            // Fixed 24KB PRG ROM at $A000-$FFFF
            else if (addr >= 0xA000) {
                region = MemoryRegion::PrgRom;
                mappedAddr = _prgFixedOffset + (addr - 0xA000);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region, uint8_t data) override {
            // Cart RAM write
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                region = MemoryRegion::PrgRam;
                mappedAddr = addr & 0x1FFF;
                return true;
            }

            // Mapper Register Writes ($A000-$FFFF)
            if (addr >= 0xA000) {
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

                // Register writes don't access memory
                region = MemoryRegion::None;
                return true;
            }

            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000) {
                region = (_chrBanks > 0) ? MemoryRegion::ChrRom : MemoryRegion::ChrRam;

                // Calculate CHR bank mask (4KB banks)
                uint32_t total4kbBanks = _chrBanks * 2; // Convert 8KB banks to 4KB banks
                uint8_t bankMask = (total4kbBanks > 0) ? (total4kbBanks - 1) : 0x1F;

                // MMC2 maps 4KB banks
                if (addr <= 0x0FFF) { // $0000-$0FFF range
                    // Select bank based on Latch 0 state
                    uint8_t bank = (_latch0 == 0) ? _chrBank0FD : _chrBank0FE;
                    mappedAddr = static_cast<uint32_t>(bank & bankMask) * 0x1000 + (addr & 0x0FFF);
                }
                else { // $1000-$1FFF range
                    // Select bank based on Latch 1 state
                    uint8_t bank = (_latch1 == 0) ? _chrBank1FD : _chrBank1FE;
                    mappedAddr = static_cast<uint32_t>(bank & bankMask) * 0x1000 + (addr & 0x0FFF);
                }

                // Update latches based on specific PPU address reads
                // Latch 0 (left pattern table) - exact addresses only
                if (addr == 0x0FD8) {
                    _latch0 = 0; // Set to FD state
                }
                else if (addr == 0x0FE8) {
                    _latch0 = 1; // Set to FE state
                }

                // Latch 1 (right pattern table) - address ranges
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

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // CHR-RAM is writable
                region = MemoryRegion::ChrRam;

                // Note: CHR-RAM typically doesn't have multiple banks with MMC2,
                // but we'll apply masking for consistency
                uint8_t bankMask = 0x1F; // Maximum banks for safety

                if (addr <= 0x0FFF) {
                    uint8_t bank = (_latch0 == 0) ? _chrBank0FD : _chrBank0FE;
                    mappedAddr = static_cast<uint32_t>(bank & bankMask) * 0x1000 + (addr & 0x0FFF);
                }
                else {
                    uint8_t bank = (_latch1 == 0) ? _chrBank1FD : _chrBank1FE;
                    mappedAddr = static_cast<uint32_t>(bank & bankMask) * 0x1000 + (addr & 0x0FFF);
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
            return false;
        }
    };
}