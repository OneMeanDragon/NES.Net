#pragma once
#include "MapperBase.h"

namespace nes {

    class Mapper001 : public MapperBase {
    public:
        static constexpr uint8_t     ID   = 1;
        static constexpr const char* NAME = "MMC1 (SxROM)";
        static constexpr const char* INFO = "Nintendo MMC1. Switchable PRG/CHR banks, serial register loading, 8KB cart RAM.";
        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }
    private:
        // Internal shift register state
        uint8_t _loadRegister = 0x00;
        uint8_t _loadCounter  = 0x00;
        uint8_t _controlReg   = 0x00;

        // Bank selection registers
        uint8_t _chrBank4Lo  = 0x00;
        uint8_t _chrBank4Hi  = 0x00;
        uint8_t _chrBank8    = 0x00;
        uint8_t _prgBank16Lo = 0x00;
        uint8_t _prgBank16Hi = 0x00;
        uint8_t _prgBank32   = 0x00;

    public:
        Mapper001(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
            // Allocate 8KB Save/Work RAM
            _cartRam.assign(8192, 0x00);
            Reset();
        }

        void Reset() override {
            _loadRegister = 0x00;
            _loadCounter = 0x00;
            _controlReg = 0x1C; // Standard MMC1 power-up state (PRG mode 3)

            _chrBank4Lo = 0;
            _chrBank4Hi = 0;
            _chrBank8 = 0;

            _prgBank32 = 0;
            _prgBank16Lo = 0;
            _prgBank16Hi = _prgBanks - 1;
        }

        // --- CPU Mapping ---

        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) override {
            // Cart RAM ($6000-$7FFF)
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                mappedAddr = 0xFFFFFFFF; // Signal Cartridge to use mapper's cartRam
                data = _cartRam[addr & 0x1FFF];
                return true;
            }

            // PRG ROM ($8000-$FFFF)
            if (addr >= 0x8000) {
                if (_controlReg & 0x08) {
                    // 16KB mode
                    if (addr < 0xC000) {
                        // $8000-$BFFF
                        mappedAddr = static_cast<uint32_t>(_prgBank16Lo) * 0x4000 + (addr & 0x3FFF);
                    }
                    else {
                        // $C000-$FFFF
                        mappedAddr = static_cast<uint32_t>(_prgBank16Hi) * 0x4000 + (addr & 0x3FFF);
                    }
                }
                else {
                    // 32KB mode
                    mappedAddr = static_cast<uint32_t>(_prgBank32) * 0x8000 + (addr & 0x7FFF);
                }
                return true;
            }
            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) override {
            // Cart RAM write
            if (addr >= 0x6000 && addr <= 0x7FFF) {
                mappedAddr = 0xFFFFFFFF;
                _cartRam[addr & 0x1FFF] = data;
                return true;
            }

            // Register writes (Shift Register)
            if (addr >= 0x8000) {
                if (data & 0x80) {
                    // Reset shift register
                    _loadRegister = 0x00;
                    _loadCounter = 0x00;
                    _controlReg |= 0x0C; // Lock PRG to mode 3
                }
                else {
                    // Load bit serially (LSB first)
                    _loadRegister = (_loadRegister >> 1) | ((data & 0x01) << 4);
                    _loadCounter++;

                    if (_loadCounter == 5) {
                        uint8_t target = (addr >> 13) & 0x03;

                        switch (target) {
                        case 0: // Control ($8000-$9FFF)
                            _controlReg = _loadRegister & 0x1F;
                            _mirrorMode = static_cast<MirrorMode>(_controlReg & 0x03);
                            break;

                        case 1: // CHR bank 0 ($A000-$BFFF)
                            if (_controlReg & 0x10) {
                                // 4KB CHR mode
                                _chrBank4Lo = _loadRegister & 0x1F;
                            }
                            else {
                                // 8KB CHR mode (ignore low bit)
                                _chrBank8 = _loadRegister & 0x1E;
                            }
                            break;

                        case 2: // CHR bank 1 ($C000-$DFFF)
                            if (_controlReg & 0x10) {
                                // 4KB CHR mode
                                _chrBank4Hi = _loadRegister & 0x1F;
                            }
                            // Else: ignored in 8KB mode
                            break;

                        case 3: // PRG bank ($E000-$FFFF)
                        {
                            uint8_t prgMode = (_controlReg >> 2) & 0x03;
                            switch (prgMode) {
                            case 0: case 1: // 32KB mode
                                _prgBank32 = (_loadRegister & 0x0E) >> 1;
                                break;
                            case 2: // Fix first bank at $8000, switch at $C000
                                _prgBank16Lo = 0;
                                _prgBank16Hi = _loadRegister & 0x0F;
                                break;
                            case 3: // Switch at $8000, fix last bank at $C000
                                _prgBank16Lo = _loadRegister & 0x0F;
                                _prgBank16Hi = _prgBanks - 1;
                                break;
                            }
                            break;
                        }
                        }
                        _loadRegister = 0x00;
                        _loadCounter = 0x00;
                    }
                }
            }
            return false;
        }

        // --- PPU Mapping ---

        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000) {
                if (_chrBanks == 0) {
                    // CHR-RAM - direct mapping, no banking
                    mappedAddr = addr;
                    return true;
                }

                // CHR-ROM - use banking
                if (_controlReg & 0x10) {
                    // 4KB CHR mode
                    if (addr < 0x1000) {
                        mappedAddr = static_cast<uint32_t>(_chrBank4Lo) * 0x1000 + (addr & 0x0FFF);
                    }
                    else {
                        mappedAddr = static_cast<uint32_t>(_chrBank4Hi) * 0x1000 + (addr & 0x0FFF);
                    }
                }
                else {
                    // 8KB CHR mode
                    mappedAddr = static_cast<uint32_t>(_chrBank8) * 0x2000 + (addr & 0x1FFF);
                }
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                mappedAddr = addr;  // Direct map for CHR-RAM
                return true;
            }
            return false;
        }
    };
}