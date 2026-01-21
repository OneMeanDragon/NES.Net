#pragma once
#include "MapperBase.h"

namespace nes {

    class Mapper206 : public MapperBase {
    public:
        static constexpr uint8_t     ID   = 206;
        static constexpr const char* NAME = "Namcot 108/118 / Tengen MIMIC-1";
        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
    private:
        uint8_t _targetRegister = 0;
        uint8_t _prgBankSelect[2] = { 0, 0 };
        uint8_t _chrBankSelect[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };

    public:
        Mapper206(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
        }

        void Reset() override {
            _targetRegister = 0;
            _prgBankSelect[0] = 0;
            _prgBankSelect[1] = 1;
            for (int i = 0; i < 8; i++) {
                _chrBankSelect[i] = i;
            }
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            if (addr >= 0x8000 && addr <= 0x9FFF) {
                // First 8KB switchable bank
                mappedAddr = (_prgBankSelect[0] * 0x2000) + (addr & 0x1FFF);
                return true;
            }
            else if (addr >= 0xA000 && addr <= 0xBFFF) {
                // Second 8KB switchable bank
                mappedAddr = (_prgBankSelect[1] * 0x2000) + (addr & 0x1FFF);
                return true;
            }
            else if (addr >= 0xC000 && addr <= 0xDFFF) {
                // Third 8KB fixed to second-to-last bank
                mappedAddr = ((_prgBanks * 2 - 2) * 0x2000) + (addr & 0x1FFF);
                return true;
            }
            else if (addr >= 0xE000 && addr <= 0xFFFF) {
                // Last 8KB fixed to last bank
                mappedAddr = ((_prgBanks * 2 - 1) * 0x2000) + (addr & 0x1FFF);
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            // Only respond to writes in $8000-$9FFF range (no $A000-$FFFF registers)
            if (addr >= 0x8000 && addr <= 0x9FFF) {
                // Even addresses: Bank select
                if ((addr & 0x01) == 0) {
                    _targetRegister = data & 0x07; // Only bits 0-2 used (8 registers)
                }
                // Odd addresses: Bank data
                else {
                    if (_targetRegister >= 0 && _targetRegister <= 5) {
                        // CHR bank registers (R0-R5)
                        // R0-R5 are 1KB CHR banks
                        _chrBankSelect[_targetRegister] = data;
                    }
                    else if (_targetRegister == 6) {
                        // PRG bank 0 (R6) - 8KB switchable
                        // Limit to max 128KB PRG (16 banks of 8KB)
                        _prgBankSelect[0] = data & 0x0F;
                    }
                    else if (_targetRegister == 7) {
                        // PRG bank 1 (R7) - 8KB switchable
                        _prgBankSelect[1] = data & 0x0F;
                    }
                }
                return false; // Don't write to cartridge memory
            }
            // No registers in $A000-$FFFF range
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                // CHR Banking: Left pattern table (0000-0FFF) uses 2KB banks (R0, R1)
                // Right pattern table (1000-1FFF) uses 1KB banks (R2-R5)

                if (addr < 0x0800) {
                    // First 2KB bank (uses R0 as 2KB)
                    mappedAddr = ((_chrBankSelect[0] & 0xFE) * 0x0400) + (addr & 0x07FF);
                }
                else if (addr < 0x1000) {
                    // Second 2KB bank (uses R1 as 2KB)
                    mappedAddr = ((_chrBankSelect[1] & 0xFE) * 0x0400) + (addr & 0x07FF);
                }
                else if (addr < 0x1400) {
                    // First 1KB bank in right pattern table (R2)
                    mappedAddr = (_chrBankSelect[2] * 0x0400) + (addr & 0x03FF);
                }
                else if (addr < 0x1800) {
                    // Second 1KB bank (R3)
                    mappedAddr = (_chrBankSelect[3] * 0x0400) + (addr & 0x03FF);
                }
                else if (addr < 0x1C00) {
                    // Third 1KB bank (R4)
                    mappedAddr = (_chrBankSelect[4] * 0x0400) + (addr & 0x03FF);
                }
                else {
                    // Fourth 1KB bank (R5)
                    mappedAddr = (_chrBankSelect[5] * 0x0400) + (addr & 0x03FF);
                }
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // Allow writes to CHR-RAM if no CHR-ROM present
                // Use same banking logic as PpuMapRead
                if (addr < 0x0800) {
                    mappedAddr = ((_chrBankSelect[0] & 0xFE) * 0x0400) + (addr & 0x07FF);
                }
                else if (addr < 0x1000) {
                    mappedAddr = ((_chrBankSelect[1] & 0xFE) * 0x0400) + (addr & 0x07FF);
                }
                else if (addr < 0x1400) {
                    mappedAddr = (_chrBankSelect[2] * 0x0400) + (addr & 0x03FF);
                }
                else if (addr < 0x1800) {
                    mappedAddr = (_chrBankSelect[3] * 0x0400) + (addr & 0x03FF);
                }
                else if (addr < 0x1C00) {
                    mappedAddr = (_chrBankSelect[4] * 0x0400) + (addr & 0x03FF);
                }
                else {
                    mappedAddr = (_chrBankSelect[5] * 0x0400) + (addr & 0x03FF);
                }
                return true;
            }
            return false;
        }
    };
}