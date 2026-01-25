#include "PPU2C02.Memory.h"
#include "PPU2C02.h"
#include "../CartridgeApi/CartridgeInterfaceAPI.h"
#include <cstring>
#include <xutility>

PPU2C02_Memory::PPU2C02_Memory() {
    std::memset(_nametable0, 0, sizeof(_nametable0));
    std::memset(_nametable1, 0, sizeof(_nametable1));
    std::memset(_nametable2, 0, sizeof(_nametable2));
    std::memset(_nametable3, 0, sizeof(_nametable3));
    std::memset(_paletteRam, 0, sizeof(_paletteRam));
    InitializeSystemPalette();
}

void PPU2C02_Memory::InitializeSystemPalette() {
    _systemPalette[0x00] = Pixel(84, 84, 84);
    _systemPalette[0x01] = Pixel(0, 30, 116);
    _systemPalette[0x02] = Pixel(8, 16, 144);
    _systemPalette[0x03] = Pixel(48, 0, 136);
    _systemPalette[0x04] = Pixel(68, 0, 100);
    _systemPalette[0x05] = Pixel(92, 0, 48);
    _systemPalette[0x06] = Pixel(84, 4, 0);
    _systemPalette[0x07] = Pixel(60, 24, 0);
    _systemPalette[0x08] = Pixel(32, 42, 0);
    _systemPalette[0x09] = Pixel(8, 58, 0);
    _systemPalette[0x0A] = Pixel(0, 64, 0);
    _systemPalette[0x0B] = Pixel(0, 60, 0);
    _systemPalette[0x0C] = Pixel(0, 50, 60);
    _systemPalette[0x0D] = Pixel(0, 0, 0);
    _systemPalette[0x0E] = Pixel(0, 0, 0);
    _systemPalette[0x0F] = Pixel(0, 0, 0);

    _systemPalette[0x10] = Pixel(152, 150, 152);
    _systemPalette[0x11] = Pixel(8, 76, 196);
    _systemPalette[0x12] = Pixel(48, 50, 236);
    _systemPalette[0x13] = Pixel(92, 30, 228);
    _systemPalette[0x14] = Pixel(136, 20, 176);
    _systemPalette[0x15] = Pixel(160, 20, 100);
    _systemPalette[0x16] = Pixel(152, 34, 32);
    _systemPalette[0x17] = Pixel(120, 60, 0);
    _systemPalette[0x18] = Pixel(84, 90, 0);
    _systemPalette[0x19] = Pixel(40, 114, 0);
    _systemPalette[0x1A] = Pixel(8, 124, 0);
    _systemPalette[0x1B] = Pixel(0, 118, 40);
    _systemPalette[0x1C] = Pixel(0, 102, 120);
    _systemPalette[0x1D] = Pixel(0, 0, 0);
    _systemPalette[0x1E] = Pixel(0, 0, 0);
    _systemPalette[0x1F] = Pixel(0, 0, 0);

    _systemPalette[0x20] = Pixel(236, 238, 236);
    _systemPalette[0x21] = Pixel(76, 154, 236);
    _systemPalette[0x22] = Pixel(120, 124, 236);
    _systemPalette[0x23] = Pixel(176, 98, 236);
    _systemPalette[0x24] = Pixel(228, 84, 236);
    _systemPalette[0x25] = Pixel(236, 88, 180);
    _systemPalette[0x26] = Pixel(236, 106, 100);
    _systemPalette[0x27] = Pixel(212, 136, 32);
    _systemPalette[0x28] = Pixel(160, 170, 0);
    _systemPalette[0x29] = Pixel(116, 196, 0);
    _systemPalette[0x2A] = Pixel(76, 208, 32);
    _systemPalette[0x2B] = Pixel(56, 204, 108);
    _systemPalette[0x2C] = Pixel(56, 180, 204);
    _systemPalette[0x2D] = Pixel(60, 60, 60);
    _systemPalette[0x2E] = Pixel(0, 0, 0);
    _systemPalette[0x2F] = Pixel(0, 0, 0);

    _systemPalette[0x30] = Pixel(236, 238, 236);
    _systemPalette[0x31] = Pixel(168, 204, 236);
    _systemPalette[0x32] = Pixel(188, 188, 236);
    _systemPalette[0x33] = Pixel(212, 178, 236);
    _systemPalette[0x34] = Pixel(236, 174, 236);
    _systemPalette[0x35] = Pixel(236, 174, 212);
    _systemPalette[0x36] = Pixel(236, 180, 176);
    _systemPalette[0x37] = Pixel(228, 196, 144);
    _systemPalette[0x38] = Pixel(204, 210, 120);
    _systemPalette[0x39] = Pixel(180, 222, 120);
    _systemPalette[0x3A] = Pixel(168, 226, 144);
    _systemPalette[0x3B] = Pixel(152, 226, 180);
    _systemPalette[0x3C] = Pixel(160, 214, 228);
    _systemPalette[0x3D] = Pixel(160, 162, 160);
    _systemPalette[0x3E] = Pixel(0, 0, 0);
    _systemPalette[0x3F] = Pixel(0, 0, 0);
}

void PPU2C02_Memory::ResetMemory(bool coldstart) {
    if (coldstart) {
        std::fill(reinterpret_cast<uint8_t*>(_paletteRam),
            reinterpret_cast<uint8_t*>(_paletteRam) + 32, 0xFF);
    }
    _dataBuffer = 0;
}

uint8_t PPU2C02_Memory::ReadNametable(uint16_t addr) {
    addr &= 0x0FFF;
    MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;

    switch (mirror) {
    case MirrorMode::Horizontal:
        if (addr < 0x0800) return _nametable0[addr & 0x03FF];
        else return _nametable1[addr & 0x03FF];
    case MirrorMode::Vertical:
        if (addr < 0x0400) return _nametable0[addr];
        else if (addr < 0x0800) return _nametable1[addr - 0x0400];
        else if (addr < 0x0C00) return _nametable0[addr - 0x0800];
        else return _nametable1[addr - 0x0C00];
    default:
        return _nametable0[addr & 0x03FF];
    }
}

void PPU2C02_Memory::WriteNametable(uint16_t addr, uint8_t data) {
    addr &= 0x0FFF;
    MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;

    switch (mirror) {
    case MirrorMode::Horizontal:
        if (addr < 0x0800) _nametable0[addr & 0x03FF] = data;
        else _nametable1[addr & 0x03FF] = data;
        break;
    case MirrorMode::Vertical:
        if (addr < 0x0400) _nametable0[addr] = data;
        else if (addr < 0x0800) _nametable1[addr - 0x0400] = data;
        else if (addr < 0x0C00) _nametable0[addr - 0x0800] = data;
        else _nametable1[addr - 0x0C00] = data;
        break;
    default:
        _nametable0[addr & 0x03FF] = data;
        break;
    }
}

uint8_t PPU2C02_Memory::PpuRead(uint16_t addr, bool rdOnly) {
    uint8_t data = 0;
    addr &= 0x3FFF;

    if (addr <= 0x1FFF) {
        if (_cart && _cart->PpuRead(addr, &data)) {
            return data;
        }
        return 0x00;
    }
    else if (addr >= 0x2000 && addr <= 0x3EFF) {
        data = ReadNametable(addr);
    }
    else if (addr >= 0x3F00 && addr <= 0x3FFF) {
        addr &= 0x1F;
        if (addr == 0x10) addr = 0x00;
        if (addr == 0x14) addr = 0x04;
        if (addr == 0x18) addr = 0x08;
        if (addr == 0x1C) addr = 0x0C;
        data = _paletteRam[addr] & 0x3F;
    }

    return data;
}

void PPU2C02_Memory::PpuWrite(uint16_t addr, uint8_t data) {
    addr &= 0x3FFF;

    if (addr <= 0x1FFF) {
        if (_cart) {
            _cart->PpuWrite(addr, data);
        }
        return;
    }
    else if (addr >= 0x2000 && addr <= 0x3EFF) {
        WriteNametable(addr, data);
    }
    else if (addr >= 0x3F00 && addr <= 0x3FFF) {
        addr &= 0x1F;
        if (addr == 0x10) addr = 0x00;
        if (addr == 0x14) addr = 0x04;
        if (addr == 0x18) addr = 0x08;
        if (addr == 0x1C) addr = 0x0C;
        _paletteRam[addr] = data & 0x3F;
    }
}

Pixel PPU2C02_Memory::GetColorFromPalette(uint8_t palette, uint8_t pixel) {
    uint16_t addr = 0x3F00;
    if (pixel != 0) {
        addr += ((uint16_t)palette << 2) + pixel;
    }
    return _systemPalette[PpuRead(addr) & 0x3F];
}