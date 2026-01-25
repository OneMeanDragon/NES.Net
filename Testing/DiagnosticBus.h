#pragma once

#include <iostream>
#include <cstdint>
#include <cstring>
#include "CPU/CPU6502.h"
#include "PPU/PPU2C02.h"
#include "APU/APU2A03.h"
#include "DiagnosticCartridge.h"

// Forward declaration - adjust based on your NESBus interface
class NESBus {
public:
    virtual ~NESBus() = default;
    virtual uint8_t CpuRead(uint16_t addr, bool rdOnly = false) = 0;
    virtual void CpuWrite(uint16_t addr, uint8_t data) = 0;
};

class DiagnosticBus : public NESBus {
private:
    uint8_t ram[2048];
    CPU6502* cpu;
    PPU2C02* ppu;
    APU2A03* apu;
    DiagnosticCartridge* cart;

public:
    DiagnosticBus() : cpu(nullptr), ppu(nullptr), apu(nullptr), cart(nullptr) {
        std::memset(ram, 0, sizeof(ram));
    }

    void ConnectCPU(CPU6502* c) { cpu = c; }
    void ConnectPPU(PPU2C02* p) { ppu = p; }
    void ConnectAPU(APU2A03* a) { apu = a; }
    void ConnectCartridge(DiagnosticCartridge* c) { cart = c; }

    uint8_t CpuRead(uint16_t addr, bool rdOnly = false) override {
        if (addr < 0x2000) {
            // CPU RAM (mirrored)

            //uint8_t result = ram[addr & 0x07FF];
            //std::cout << "CPU Read: Addr=" << std::hex << addr
            //    << " Data=" << std::hex << +result << std::endl;

            return ram[addr & 0x07FF];
        }
        else if (addr >= 0x2000 && addr < 0x4000) {
            // PPU registers (mirrored)
            return ppu ? ppu->CpuRead(addr & 0x0007, rdOnly) : 0;
        }
        else if (addr >= 0x4000 && addr < 0x4018) {
            // APU and I/O registers
            if (addr == 0x4015) {
                return apu ? apu->CpuRead(addr) : 0xFF;
            }
            // Controller reads would go here (0x4016, 0x4017)
            return 0xff;
        }
        else if (addr >= 0x6000 && addr < 0x8000) {
            // Cartridge RAM (if present)
            return 0;
        }
        else if (addr >= 0x8000) {
            // Cartridge ROM
            return cart ? cart->CpuRead(addr, nullptr) : 0;
        }
        return 0;
    }

    void CpuWrite(uint16_t addr, uint8_t data) override {
        if (addr < 0x2000) {
            // CPU RAM (mirrored)
            //std::cout << "CPU Write: Addr=" << std::hex << addr
            //    << " Data=" << std::hex << +data << std::endl;

            ram[addr & 0x07FF] = data;
        }
        else if (addr >= 0x2000 && addr < 0x4000) {
            // PPU registers (mirrored)
            if (ppu) ppu->CpuWrite(addr & 0x0007, data);
        }
        else if (addr == 0x4014) {
            // PPU OAM DMA
            if (ppu) {
                // DMA transfer
                uint16_t page = data << 8;
                for (uint16_t i = 0; i < 256; i++) {
                    uint8_t byte = CpuRead(page + i);
                    ppu->CpuWrite(0x2004, byte);
                }
            }
        }
        else if (addr >= 0x4000 && addr < 0x4018) {
            // APU and I/O registers
            if (apu) apu->CpuWrite(addr, data);
        }
        else if (addr >= 0x6000 && addr < 0x8000) {
            // Cartridge RAM (if present)
        }
        else if (addr >= 0x8000) {
            // Cartridge ROM (mapper writes)
            if (cart) cart->CpuWrite(addr, data);
        }
    }

    // Direct RAM access for testing
    uint8_t ReadRAM(uint16_t addr) {
        if (addr < 2048) {
            return ram[addr];
        }
        return 0;
    }

    void WriteRAM(uint16_t addr, uint8_t data) {
        if (addr < 2048) {
            ram[addr] = data;
        }
    }
};