#pragma once
#include "MapperBase.h"
#include "../Cartridge.h"
#include <cstring>
#include <cstdio>

namespace nes {

    class Mapper206 : public MapperBase {
    public:
        static constexpr uint8_t     ID = 206;
        static constexpr const char* NAME = "Namcot 108/118 / Tengen MIMIC-1";
        static constexpr const char* INFO = "Namco 118, Tengen MIMIC-1 (Discrete MMC3)";

        virtual constexpr uint8_t GetMapperNumber() const noexcept override { return ID; }
        virtual constexpr const char* GetMapperName() const noexcept override { return NAME; }
        virtual constexpr const char* GetMapperInfo() const noexcept override { return INFO; }

    private:
        // 8 registers like MMC3
        uint8_t _registers[8]{ 0 };
        uint8_t _targetRegister = 0;

        // Control flags
        bool _prgBankMode = false;
        bool _chrInversion = false;

        // Cached bank addresses for faster access
        uint32_t _prgBankOffsets[4]{ 0 };
        uint32_t _chrBankOffsets[8]{ 0 };

        // IRQ
        uint8_t _irqCounter = 0;
        uint8_t _irqReload = 0;
        bool _irqEnable = false;
        bool _irqActive = false;
        bool _irqReloadFlag = false;

        // Debug
        bool _debug = false;

        void UpdateBanks() {
            // --- Update PRG banks (4 x 8KB) ---
            uint32_t total8KBBanks = _prgBanks * 2;
            uint32_t last8KBBank = total8KBBanks - 1;
            uint32_t secondLast8KBBank = total8KBBanks - 2;

            // Mask for available banks
            uint8_t bankMask = (total8KBBanks - 1) & 0x3F;

            if (_prgBankMode) {
                // Mode 1: $8000-$9FFF fixed to second-last, $C000-$DFFF switchable
                _prgBankOffsets[0] = secondLast8KBBank * 0x2000;
                _prgBankOffsets[1] = (_registers[7] & bankMask) * 0x2000;
                _prgBankOffsets[2] = (_registers[6] & bankMask) * 0x2000;
                _prgBankOffsets[3] = last8KBBank * 0x2000;
            }
            else {
                // Mode 0: $8000-$9FFF switchable, $C000-$DFFF fixed to second-last
                _prgBankOffsets[0] = (_registers[6] & bankMask) * 0x2000;
                _prgBankOffsets[1] = (_registers[7] & bankMask) * 0x2000;
                _prgBankOffsets[2] = secondLast8KBBank * 0x2000;
                _prgBankOffsets[3] = last8KBBank * 0x2000;
            }

            // --- Update CHR banks (8 x 1KB) ---
            uint32_t total1KBBanks = _chrBanks * 8;
            uint32_t chrMask = total1KBBanks - 1;

            if (_chrInversion) {
                // Inverted: Two 2KB banks at $1000-$1FFF, four 1KB at $0000-$0FFF
                _chrBankOffsets[0] = (_registers[2] & chrMask) * 0x0400;
                _chrBankOffsets[1] = (_registers[3] & chrMask) * 0x0400;
                _chrBankOffsets[2] = (_registers[4] & chrMask) * 0x0400;
                _chrBankOffsets[3] = (_registers[5] & chrMask) * 0x0400;
                _chrBankOffsets[4] = ((_registers[0] & 0xFE) & chrMask) * 0x0400;
                _chrBankOffsets[5] = ((_registers[0] | 0x01) & chrMask) * 0x0400;
                _chrBankOffsets[6] = ((_registers[1] & 0xFE) & chrMask) * 0x0400;
                _chrBankOffsets[7] = ((_registers[1] | 0x01) & chrMask) * 0x0400;
            }
            else {
                // Normal: Two 2KB banks at $0000-$0FFF, four 1KB at $1000-$1FFF
                _chrBankOffsets[0] = ((_registers[0] & 0xFE) & chrMask) * 0x0400;
                _chrBankOffsets[1] = ((_registers[0] | 0x01) & chrMask) * 0x0400;
                _chrBankOffsets[2] = ((_registers[1] & 0xFE) & chrMask) * 0x0400;
                _chrBankOffsets[3] = ((_registers[1] | 0x01) & chrMask) * 0x0400;
                _chrBankOffsets[4] = (_registers[2] & chrMask) * 0x0400;
                _chrBankOffsets[5] = (_registers[3] & chrMask) * 0x0400;
                _chrBankOffsets[6] = (_registers[4] & chrMask) * 0x0400;
                _chrBankOffsets[7] = (_registers[5] & chrMask) * 0x0400;
            }

            if (_debug) {
                printf("Mapper206: UpdateBanks\n");
                printf("  PRG Mode: %d, CHR Inv: %d\n", _prgBankMode, _chrInversion);
                printf("  PRG Offsets: %08X %08X %08X %08X\n",
                    _prgBankOffsets[0], _prgBankOffsets[1],
                    _prgBankOffsets[2], _prgBankOffsets[3]);
            }
        }

    public:
        Mapper206(uint8_t prgBanks, uint8_t chrBanks)
            : MapperBase(prgBanks, chrBanks) {
            Reset();
        }

        void Reset() override {
            _targetRegister = 0;
            _prgBankMode = false;
            _chrInversion = false;
            _mirrorMode = MirrorMode::Vertical;

            _irqCounter = 0;
            _irqReload = 0;
            _irqEnable = false;
            _irqActive = false;
            _irqReloadFlag = false;

            std::memset(_registers, 0, sizeof(_registers));

            // Set initial banks
            _registers[6] = 0;
            _registers[7] = 1;

            UpdateBanks();

            if (_debug) {
                printf("Mapper206: Reset\n");
                printf("  PRG banks: %d, CHR banks: %d\n", _prgBanks, _chrBanks);
            }
        }

        // --- CPU Mapping ---
        bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            // PRG-ROM reads ($8000-$FFFF)
            if (addr >= 0x8000 && addr <= 0xFFFF) {
                region = MemoryRegion::PrgRom;
                int index = (addr >> 13) & 0x03;
                mappedAddr = _prgBankOffsets[index] + (addr & 0x1FFF);
                return true;
            }

            return false;
        }

        bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region, uint8_t data) override {
            if (_debug && addr >= 0x8000) {
                printf("Mapper206: Write to $%04X = $%02X\n", addr, data);
            }

            // Mapper 206 has NO cart RAM, only registers
            if (addr >= 0x8000 && addr <= 0xFFFF) {
                // Bank select/data ($8000-$9FFF)
                if (addr >= 0x8000 && addr <= 0x9FFF) {
                    if ((addr & 1) == 0) {
                        _targetRegister = data & 0x07;
                        _prgBankMode = (data & 0x40) != 0;
                        _chrInversion = (data & 0x80) != 0;
                    }
                    else {
                        _registers[_targetRegister] = data;
                        UpdateBanks();
                    }
                }
                // Mirroring ($A000-$BFFF)
                else if (addr >= 0xA000 && addr <= 0xBFFF) {
                    if ((addr & 1) == 0) {
                        _mirrorMode = (data & 1) ? MirrorMode::Horizontal : MirrorMode::Vertical;
                    }
                }
                // IRQ ($C000-$FFFF)
                else if (addr >= 0xC000 && addr <= 0xDFFF) {
                    if ((addr & 1) == 0) {
                        _irqReload = data;
                    }
                    else {
                        _irqReloadFlag = true;
                    }
                }
                else if (addr >= 0xE000 && addr <= 0xFFFF) {
                    if ((addr & 1) == 0) {
                        _irqEnable = false;
                        _irqActive = false;
                    }
                    else {
                        _irqEnable = true;
                    }
                }

                region = MemoryRegion::None;
                return true;
            }

            return false;
        }

        // --- PPU Mapping ---
        bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000) {
                region = (_chrBanks > 0) ? MemoryRegion::ChrRom : MemoryRegion::ChrRam;
                // CHR banking - 1KB banks
                uint8_t bankIndex = (addr >> 10) & 0x07;
                mappedAddr = _chrBankOffsets[bankIndex] + (addr & 0x03FF);

                if (_debug && addr < 0x0100) {
                    printf("Mapper206: Read CHR $%04X -> bank %d, offset %08X\n",
                        addr, bankIndex, mappedAddr);
                }
                return true;
            }
            return false;
        }

        bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) override {
            if (addr < 0x2000 && _chrBanks == 0) {
                // CHR-RAM is writable
                region = MemoryRegion::ChrRam;
                uint8_t bankIndex = (addr >> 10) & 0x07;
                mappedAddr = _chrBankOffsets[bankIndex] + (addr & 0x03FF);
                return true;
            }
            return false;
        }

        // --- IRQ Functions ---
        bool IsIrqActive() const override { return _irqActive; }
        void ClearIrq() override { _irqActive = false; }

        void ScanlineCounter() override {
            if (_irqReloadFlag) {
                _irqCounter = _irqReload;
                _irqReloadFlag = false;
            }
            else if (_irqCounter == 0) {
                _irqCounter = _irqReload;
            }
            else {
                _irqCounter--;
            }

            if (_irqCounter == 0 && _irqEnable) {
                _irqActive = true;
            }
        }

        MirrorMode GetMirrorMode() const override {
            return _mirrorMode;
        }
    };
}