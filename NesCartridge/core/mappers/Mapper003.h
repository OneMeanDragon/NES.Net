#pragma once
#include "MapperBase.h"
#include "../Cartridge.h"

namespace nes {

    class Mapper003 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 3;
        static constexpr const char* NAME = "CNROM";
        static constexpr const char* INFO = "CNROM. Fixed PRG, switchable 8KB CHR banks.";

        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

    private:
        uint8_t _chrBank = 0;

    public:
        Mapper003(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
            Reset();
        }

        void Reset() override {
            _chrBank = 0;
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr >= 0x8000) {
                region = MemoryRegion::PrgRom;
                // Like Mapper 000: 
                // 16KB PRG (1 bank): Mirror ($8000-$BFFF mirrors to $C000-$FFFF)
                // 32KB PRG (2 banks): Direct Map
                mappedAddr = (_prgBanks == 1) ? (addr & 0x3FFF) : (addr & 0x7FFF);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region, uint8_t data) override {
            if (addr >= 0x8000) {
                // CNROM bank select: Writing to ROM space selects an 8KB CHR bank
                if (_chrBanks > 1) {
                    _chrBank = data & (_chrBanks - 1);
                }
                else {
                    _chrBank = 0;
                }
                // Register write, not memory access
                region = MemoryRegion::None;
                return true;
            }
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000) {
                // Select 8KB CHR bank based on the _chrBank register
                mappedAddr = static_cast<uint32_t>(_chrBank) * 0x2000 + addr;
                region = (_chrBanks > 0) ? MemoryRegion::ChrRom : MemoryRegion::ChrRam;
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            // CNROM uses CHR-ROM; typically not writable
            if (addr < 0x2000 && _chrBanks == 0) {
                // Only writable if using CHR-RAM
                region = MemoryRegion::ChrRam;
                mappedAddr = addr;
                return true;
            }
            return false;
        }
    };

}