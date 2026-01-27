#pragma once
#include "MapperBase.h"
#include "../Cartridge.h" // For MemoryRegion enum

namespace nes {

    class Mapper000 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 0;
        static constexpr const char* NAME = "NROM";
        static constexpr const char* INFO = "No mapper - simple direct mapping. 16KB or 32KB PRG, up to 8KB CHR.";

        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

    public:
        Mapper000(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
        }

        void Reset() override {
            // Nothing to reset for NROM
        }

        // --- CPU Mapping ---
        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            // PRG-RAM: $6000-$7FFF (rare, but some NROM carts have it)
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                region = MemoryRegion::PrgRam;
                mappedAddr = addr & 0x1FFF;
                return true;
            }
            // PRG-ROM: $8000-$FFFF
            else if (addr >= 0x8000) {
                region = MemoryRegion::PrgRom;
                // If 2+ PRG banks (32KB), use full range, otherwise mirror first 16KB
                mappedAddr = (_prgBanks > 1) ? (addr & 0x7FFF) : (addr & 0x3FFF);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region, uint8_t data) override {
            // PRG-RAM: $6000-$7FFF (if cartridge has RAM allocated)
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                region = MemoryRegion::PrgRam;
                mappedAddr = addr & 0x1FFF;
                return true;
            }
            // NROM has no other writable regions
            return false;
        }

        // --- PPU Mapping ---
        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000) {
                mappedAddr = addr;

                // If only 1 CHR bank (8KB), mirror the second pattern table
                //if (_chrBanks == 1) {
                //    mappedAddr &= 0x1FFF;  // Wrap 0x2000 range into 0x1FFF
                //}

                // Use CHR-ROM if present, otherwise CHR-RAM
                region = (_chrBanks > 0) ? MemoryRegion::ChrRom : MemoryRegion::ChrRam;
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // Only allow writes to CHR-RAM (when no CHR-ROM)
                mappedAddr = addr;
                region = MemoryRegion::ChrRam;
                return true;
            }
            return false;
        }
    };
}