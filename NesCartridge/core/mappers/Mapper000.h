#pragma once
#include "MapperBase.h" // Assuming the base class is defined here

namespace nes {

    class Mapper000 : public MapperBase {
    public:
        static constexpr uint8_t     ID   = 0;
        static constexpr const char* NAME = "NROM";
        static constexpr const char* INFO = "No mapper - simple direct mapping. 16KB or 32KB PRG, up to 8KB CHR.";
        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }
    public:
        // Constructor matches the base class required parameters
        Mapper000(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
            // NROM often uses CHR-RAM if no CHR-ROM banks are present.
        }

        void Reset() override {
            // Nothing to reset for NROM
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x8000) {
                // If 2 PRG banks are present (>1), use all 32KB (AND 0x7FFF), 
                // otherwise mirror the first 16KB (AND 0x3FFF).
                mappedAddr = (_prgBanks > 1) ? (addr & 0x7FFF) : (addr & 0x3FFF);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            // NROM has no PRG RAM or registers to write to in this range
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                // CHR address space is always direct map
                mappedAddr = addr;
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                if (_chrBanks == 0) {
                    // Only allow writes if using CHR-RAM (no CHR-ROM banks loaded)
                    mappedAddr = addr;
                    return true;
                }
            }
            return false;
        }
    };
}
