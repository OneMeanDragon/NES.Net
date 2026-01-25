#pragma once
#include "MapperBase.h"

namespace nes {

    class Mapper002 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 2;
        static constexpr const char* NAME = "UxROM";
        static constexpr const char* INFO = "UxROM: Switchable 16KB PRG banks, fixed CHR.";

        // Mapper info
        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

    private:
        uint8_t _prgBankLo = 0; // Switchable bank ($8000-$BFFF)
        uint8_t _prgBankHi = 0; // Fixed last bank ($C000-$FFFF)

    public:
        Mapper002(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks)
        {
            Reset();
        }

        void Reset() override {
            _prgBankLo = 0;
            _prgBankHi = _prgBanks - 1; // Always fixed to last PRG bank
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x8000 && addr <= 0xBFFF) {
                // Switchable bank
                mappedAddr = _prgBankLo * 0x4000 + (addr & 0x3FFF);
                return true;
            }
            else if (addr >= 0xC000) {
                // Fixed last bank
                mappedAddr = _prgBankHi * 0x4000 + (addr & 0x3FFF);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            if (addr >= 0x8000) {
                // Writing to ROM space switches the low PRG bank
                _prgBankLo = data & (_prgBanks - 1);
            }
            // Not actually writing to memory
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                // UxROM uses fixed 8KB CHR bank
                mappedAddr = addr;
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // Allow writes only if CHR-RAM
                mappedAddr = addr;
                return true;
            }
            return false;
        }
    };

} // namespace nes
