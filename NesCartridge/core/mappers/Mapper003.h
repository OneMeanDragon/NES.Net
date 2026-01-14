#pragma once
#include "MapperBase.h"

namespace nes {

    class Mapper003 : public MapperBase {
    private:
        uint8_t _chrBank = 0;

    public:
        Mapper003(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
            Reset();
        }

        uint8_t GetMapperNumber() const override { return 3; }
        std::string GetMapperName() const override { return "CNROM"; }

        void Reset() override {
            _chrBank = 0;
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x8000) {
                // Like Mapper 000: 
                // 16KB PRG (1 bank): Mirror ($8000-$BFFF mirrors to $C000-$FFFF)
                // 32KB PRG (2 banks): Direct Map
                mappedAddr = (_prgBanks == 1) ? (addr & 0x3FFF) : (addr & 0x7FFF);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            if (addr >= 0x8000) {
                // CNROM bank select: Writing to ROM space selects a 8KB CHR bank.
                // It usually only uses the lowest 2 bits (supporting up to 4 banks / 32KB CHR).
                _chrBank = data & 0x03;
            }
            // Return false as no actual PRG data is being written
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                // Select 8KB CHR bank based on the _chrBank register
                mappedAddr = static_cast<uint32_t>(_chrBank) * 0x2000 + addr;
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            // CNROM uses CHR-ROM; typically, CHR-ROM is not writable by the PPU.
            return false;
        }
    };

}
