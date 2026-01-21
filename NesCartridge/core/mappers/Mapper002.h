#pragma once
#include "MapperBase.h"

namespace nes {

    class Mapper002 : public MapperBase {
    public:
        static constexpr uint8_t     ID   = 2;
        static constexpr const char* NAME = "UxROM";
        static constexpr const char* INFO = "UxROM. Switchable 16KB PRG banks, fixed CHR.";
        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }
    private:
        uint8_t _prgBankLo = 0;
        uint8_t _prgBankHi = 0;

    public:
        Mapper002(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
            Reset();
        }

        void Reset() override {
            _prgBankLo = 0;
            // High bank is fixed to the last bank in PRG ROM
            _prgBankHi = _prgBanks - 1;
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x8000 && addr <= 0xBFFF) {
                // Switchable bank ($8000-$BFFF)
                mappedAddr = static_cast<uint32_t>(_prgBankLo) * 0x4000 + (addr & 0x3FFF);
                return true;
            }
            else if (addr >= 0xC000) {
                // Fixed to last bank ($C000-$FFFF)
                mappedAddr = static_cast<uint32_t>(_prgBankHi) * 0x4000 + (addr & 0x3FFF);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            // Writing to ROM space ($8000-$FFFF) triggers a bank switch
            if (addr >= 0x8000) {
                // UxROM uses the data written to determine the new low bank
                // Masking with (prgBanks - 1) handles varying ROM sizes (UxROM/UOROM)
                _prgBankLo = data & (_prgBanks - 1);
            }

            // Return false because we are not writing to memory, only updating a register
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                // UxROM typically uses 8KB of fixed CHR (usually CHR-RAM)
                mappedAddr = addr;
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // Allow writes only if using CHR-RAM
                mappedAddr = addr;
                return true;
            }
            return false;
        }
    };

}
