#pragma once
#include "MapperBase.h"
#include "../Cartridge.h"

namespace nes {

    class Mapper066 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 66;
        static constexpr const char* NAME = "GxROM";
        static constexpr const char* INFO = "GxROM. Simple 32KB PRG + 8KB CHR banking.";

        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

    private:
        uint8_t _prgBank = 0;
        uint8_t _chrBank = 0;

    public:
        Mapper066(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
            Reset();
        }

        void Reset() override {
            _chrBank = 0;
            // GxROM uses 32KB banks, _prgBanks is in 16KB units
            if (_prgBanks >= 2) {
                _prgBank = (_prgBanks / 2) - 1;
            }
            else {
                _prgBank = 0;
            }
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr >= 0x8000) {
                region = MemoryRegion::PrgRom;
                // 32KB banking logic
                mappedAddr = static_cast<uint32_t>(_prgBank) * 0x8000 + (addr & 0x7FFF);

                // Manual bounds check (mirroring if address exceeds size)
                uint32_t maxAddr = static_cast<uint32_t>(_prgBanks) * 0x4000;
                if (mappedAddr >= maxAddr) {
                    mappedAddr %= maxAddr;
                }

                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region, uint8_t data) override {
            if (addr >= 0x8000) {
                // GxROM register format: [..PP..CC]
                // CC: CHR bank select (lower 2 bits)
                // PP: PRG bank select (bits 4 and 5)
                _chrBank = data & 0x03;
                _prgBank = (data >> 4) & 0x03;

                // Register write, not memory access
                region = MemoryRegion::None;
                return true;
            }
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000) {
                // Switchable 8KB CHR bank
                region = (_chrBanks > 0) ? MemoryRegion::ChrRom : MemoryRegion::ChrRam;

                // Apply CHR bank masking
                uint8_t maskedBank = (_chrBanks > 0) ? (_chrBank & (_chrBanks - 1)) : _chrBank;
                mappedAddr = static_cast<uint32_t>(maskedBank) * 0x2000 + addr;
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // CHR-RAM is writable
                region = MemoryRegion::ChrRam;
                // For CHR-RAM, banking usually doesn't apply but we mask for safety
                mappedAddr = static_cast<uint32_t>(_chrBank & 0x03) * 0x2000 + addr;
                return true;
            }
            return false;
        }
    };
}