#pragma once
#include "MapperBase.h"

namespace nes {

    class Mapper066 : public MapperBase {
    private:
        uint8_t _prgBank = 0;
        uint8_t _chrBank = 0;

    public:
        Mapper066(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
            Reset();
        }

        uint8_t GetMapperNumber() const override { return 66; }
        std::string GetMapperName() const override { return "GxROM"; }

        void Reset() override {
            _chrBank = 0;
            // VB.NET logic: _prgBanks is in 16KB units. 
            // GxROM uses 32KB banks, so we divide by 2.
            if (_prgBanks >= 2) {
                _prgBank = (_prgBanks / 2) - 1;
            }
            else {
                _prgBank = 0;
            }
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x8000) {
                // 32KB banking logic
                mappedAddr = static_cast<uint32_t>(_prgBank) * 0x8000 + (addr & 0x7FFF);

                // Manual bounds check (mirroring if address exceeds file size)
                uint32_t maxAddr = static_cast<uint32_t>(_prgBanks) * 16384;
                if (mappedAddr >= maxAddr) {
                    mappedAddr %= maxAddr;
                }

                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            if (addr >= 0x8000) {
                // GxROM register format: [..PP..CC]
                // CC: CHR bank select (lower 2 bits)
                // PP: PRG bank select (bits 4 and 5)
                _chrBank = data & 0x03;
                _prgBank = (data >> 4) & 0x03;
            }
            // Return false: writing to mapper registers, not memory
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                // Switchable 8KB CHR bank
                mappedAddr = static_cast<uint32_t>(_chrBank) * 0x2000 + addr;
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            // GxROM is almost exclusively CHR-ROM based
            return false;
        }
    };
}
