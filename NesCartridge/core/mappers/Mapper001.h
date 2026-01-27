#pragma once
#include "MapperBase.h"
#include "../Cartridge.h"

namespace nes {

    class Mapper001 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 1;
        static constexpr const char* NAME = "MMC1 (SxROM)";
        static constexpr const char* INFO = "Nintendo MMC1. Serial register, PRG/CHR banking, 8KB cart RAM.";

        constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        constexpr const char* GetMapperName() const noexcept override { return NAME; }
        constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

    private:
        // --- Shift register ---
        uint8_t _loadRegister = 0;
        uint8_t _loadCounter = 0;

        // --- MMC1 registers ---
        uint8_t _controlReg = 0x1C;   // Power-up default

        // --- CHR banking ---
        uint8_t _chrBank4Lo = 0;
        uint8_t _chrBank4Hi = 0;
        uint8_t _chrBank8 = 0;

        // --- PRG banking ---
        uint8_t _prgBank16Lo = 0;
        uint8_t _prgBank16Hi = 0;
        uint8_t _prgBank32 = 0;

        // --- Submapper support ---
        uint8_t _submapper = 0;
        uint8_t _prgBankMask = 0x0F;  // Default: 4 bits (16 banks)

    public:
        Mapper001(uint8_t prgBanks, uint8_t chrBanks, uint8_t submapper = 0)
            : MapperBase(prgBanks, chrBanks), _submapper(submapper)
        {
            // Configure based on submapper
            switch (_submapper) {
            case 1: // SUROM - 512KB PRG-ROM support (32 banks)
                _prgBankMask = 0x1F;  // 5 bits for PRG banking
                break;
            case 5: // SXROM - 32KB PRG-RAM with battery
                _prgBankMask = 0x0F;
                // Note: Cartridge should allocate 32KB RAM based on header
                break;
            default: // Standard MMC1
                _prgBankMask = 0x0F;  // 4 bits (16 banks)
                break;
            }

            Reset();
        }

        void Reset() override {
            _loadRegister = 0;
            _loadCounter = 0;
            _controlReg = 0x1C;   // PRG mode 3, vertical mirroring

            _chrBank4Lo = 0;
            _chrBank4Hi = 0;
            _chrBank8 = 0;

            _prgBank32 = 0;
            _prgBank16Lo = 0;
            _prgBank16Hi = _prgBanks - 1;
        }

        // ============================================================
        // CPU MAP
        // ============================================================

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            // Cartridge RAM ($6000–$7FFF)
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                region = MemoryRegion::PrgRam;
                mappedAddr = addr & 0x1FFF; // 8KB window
                return true;
            }

            // PRG ROM ($8000–$FFFF)
            if (addr >= 0x8000) {
                region = MemoryRegion::PrgRom;

                // Calculate bank mask based on available PRG banks
                uint8_t bankMask16KB = _prgBanks - 1;
                uint8_t bankMask32KB = (_prgBanks / 2) - 1;

                if (_controlReg & 0x08) {
                    // 16KB mode
                    if (addr < 0xC000) {
                        mappedAddr = ((_prgBank16Lo & bankMask16KB) * 0x4000) + (addr & 0x3FFF);
                    }
                    else {
                        mappedAddr = ((_prgBank16Hi & bankMask16KB) * 0x4000) + (addr & 0x3FFF);
                    }
                }
                else {
                    // 32KB mode
                    mappedAddr = ((_prgBank32 & bankMask32KB) * 0x8000) + (addr & 0x7FFF);
                }
                return true;
            }

            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region, uint8_t data) override {
            // Cartridge RAM
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                region = MemoryRegion::PrgRam;
                mappedAddr = addr & 0x1FFF;
                return true;
            }

            // MMC1 register writes
            if (addr >= 0x8000) {
                if (data & 0x80) {
                    // Reset shift register
                    _loadRegister = 0;
                    _loadCounter = 0;
                    _controlReg |= 0x0C; // Force PRG mode 3
                }
                else {
                    // Shift in LSB
                    _loadRegister = (_loadRegister >> 1) | ((data & 0x01) << 4);
                    _loadCounter++;

                    if (_loadCounter == 5) {
                        WriteRegister(addr);
                        _loadRegister = 0;
                        _loadCounter = 0;
                    }
                }

                // Register writes don't access memory
                region = MemoryRegion::None;
                return true;
            }

            return false;
        }

        // ============================================================
        // PPU MAP
        // ============================================================

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000) {
                if (_chrBanks == 0) {
                    // CHR-RAM
                    region = MemoryRegion::ChrRam;
                    mappedAddr = addr;
                }
                else {
                    // CHR-ROM with banking
                    region = MemoryRegion::ChrRom;

                    // Calculate bank mask based on available CHR banks
                    // For 4KB banks: _chrBanks * 2 (since each 8KB bank = 2x 4KB banks)
                    uint8_t bankMask4KB = (_chrBanks * 2) - 1;
                    uint8_t bankMask8KB = _chrBanks - 1;

                    if (_controlReg & 0x10) {
                        // 4KB CHR mode
                        if (addr < 0x1000) {
                            mappedAddr = ((_chrBank4Lo & bankMask4KB) * 0x1000) + (addr & 0x0FFF);
                        }
                        else {
                            mappedAddr = ((_chrBank4Hi & bankMask4KB) * 0x1000) + (addr & 0x0FFF);
                        }
                    }
                    else {
                        // 8KB CHR mode
                        mappedAddr = ((_chrBank8 & bankMask8KB) * 0x2000) + (addr & 0x1FFF);
                    }
                }
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // CHR-RAM is writable
                region = MemoryRegion::ChrRam;
                mappedAddr = addr;
                return true;
            }
            return false;
        }

    private:
        void WriteRegister(uint16_t addr) {
            uint8_t target = (addr >> 13) & 0x03;

            switch (target) {
            case 0: // Control
                _controlReg = _loadRegister & 0x1F;
                _mirrorMode = static_cast<MirrorMode>(_controlReg & 0x03);
                break;

            case 1: // CHR bank 0
                if (_controlReg & 0x10)
                    _chrBank4Lo = _loadRegister & 0x1F;
                else
                    _chrBank8 = _loadRegister & 0x1E;
                break;

            case 2: // CHR bank 1
                if (_controlReg & 0x10)
                    _chrBank4Hi = _loadRegister & 0x1F;
                break;

            case 3: // PRG bank
            {
                uint8_t prgBankValue = _loadRegister & _prgBankMask;

                switch ((_controlReg >> 2) & 0x03) {
                case 0:
                case 1:
                    // 32KB mode - use upper bits
                    _prgBank32 = (prgBankValue & 0x0E) >> 1;
                    break;
                case 2:
                    // Fix first to bank 0, switch upper
                    _prgBank16Lo = 0;
                    _prgBank16Hi = prgBankValue;
                    break;
                case 3:
                    // Switch lower, fix upper to last
                    _prgBank16Lo = prgBankValue;
                    _prgBank16Hi = _prgBanks - 1;
                    break;
                }
            }
            break;
            }
        }
    };

} // namespace nes