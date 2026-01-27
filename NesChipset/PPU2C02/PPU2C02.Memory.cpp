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
    std::memset(_paletteRam, 0xFF, sizeof(_paletteRam));
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

    //// DEBUG: Check what we're reading
    //if ((addr & 0x0FFF) >= 0x0380 && (addr & 0x0FFF) <= 0x03BF) {
    //    uint16_t offset = addr & 0x03FF;
    //    printf("READ NAMETABLE: addr=%04X offset=%04X \n", addr, offset);
    //}

    /*
        Horz and Vert (fixed Ms. Pacman and broke everything else)
        This fix extended into write aswell.
    */
    switch (mirror) {
    case MirrorMode::Horizontal:
        // Horizontal: $2000/$2400 use NT0, $2800/$2C00 use NT1
        //if (addr < 0x0800)
        if ((addr & 0x0400) == 0)  // Check left/right (bit 10)
            return _nametable0[addr & 0x03FF];
        else
            return _nametable1[addr & 0x03FF];

    case MirrorMode::Vertical:
        // Vertical: $2000/$2800 use NT0, $2400/$2C00 use NT1
        //if ((addr & 0x0400) == 0)
        if (addr < 0x0800)  // Check top/bottom (bit 11)
            return _nametable0[addr & 0x03FF];
        else
            return _nametable1[addr & 0x03FF];

    case MirrorMode::FourScreen:
        if (addr < 0x0400) return _nametable0[addr];
        else if (addr < 0x0800) return _nametable1[addr - 0x0400];
        else if (addr < 0x0C00) return _nametable2[addr - 0x0800];
        else return _nametable3[addr - 0x0C00];

    default:
        return _nametable0[addr & 0x03FF];
    }
}

void PPU2C02_Memory::WriteNametable(uint16_t addr, uint8_t data) {
    addr &= 0x0FFF;
    MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;

    // DEBUG
    //uint16_t coarseY = (addr >> 5) & 0x1F;
    //if (coarseY >= 28) {
    //    printf("WRITE NAMETABLE: addr=%04X data=%02X coarseY=%d\n", addr, data, coarseY);
    //}

    switch (mirror) {
    case MirrorMode::Horizontal:
        //if (addr < 0x0800)
        if ((addr & 0x0400) == 0)  // Check left/right (bit 10)
            _nametable0[addr & 0x03FF] = data;
        else
            _nametable1[addr & 0x03FF] = data;
        break;

    case MirrorMode::Vertical:
        //if ((addr & 0x0400) == 0)
        if (addr < 0x0800)  // Check top/bottom (bit 11)
            _nametable0[addr & 0x03FF] = data;
        else
            _nametable1[addr & 0x03FF] = data;
        break;

    case MirrorMode::FourScreen:
        if (addr < 0x0400) _nametable0[addr] = data;
        else if (addr < 0x0800) _nametable1[addr - 0x0400] = data;
        else if (addr < 0x0C00) _nametable2[addr - 0x0800] = data;
        else _nametable3[addr - 0x0C00] = data;
        break;

    default:
        _nametable0[addr & 0x03FF] = data;
        break;
    }
}

uint8_t PPU2C02_Memory::PpuRead(uint16_t addr, bool rdOnly) {
    uint8_t data = 0;
    addr &= 0x3FFF; // Mirror 16KB PPU space

    // --- CHR-ROM / Pattern tables ---
    if (addr <= 0x1FFF) {
        if (_cart && _cart->PpuRead(addr, &data)) {
            return data;
        }
        return 0x00;
    }

    // --- Nametables (mirroring aware) ---
    if (addr >= 0x2000 && addr <= 0x3EFF) { // > 1fff < 3f00
        return ReadNametable(addr);
    }

    // --- Palette memory ---
    if (addr >= 0x3F00 && addr <= 0x3FFF) {
        uint16_t palAddr = addr & 0x1F;

        // Mirror background colors
        if (palAddr == 0x10) palAddr = 0x00;
        if (palAddr == 0x14) palAddr = 0x04;
        if (palAddr == 0x18) palAddr = 0x08;
        if (palAddr == 0x1C) palAddr = 0x0C;

        return _paletteRam[palAddr] & 0x3F;
    }

    return 0x00;
}

void PPU2C02_Memory::PpuWrite(uint16_t addr, uint8_t data) {
    addr &= 0x3FFF; // Mirror 16KB PPU space

    // --- CHR-ROM / Pattern tables ---
    if (addr <= 0x1FFF) {
        if (_cart) {
            _cart->PpuWrite(addr, data);
        }
        return;
    }

    // --- Nametables (mirroring aware) ---
    if (addr >= 0x2000 && addr <= 0x3EFF) {
        WriteNametable(addr, data);
        return;
    }

    // --- Palette memory ---
    if (addr >= 0x3F00 && addr <= 0x3FFF) {
        addr &= 0x1F;

        // Mirror background colors
        if (addr == 0x10) addr = 0x00;
        if (addr == 0x14) addr = 0x04;
        if (addr == 0x18) addr = 0x08;
        if (addr == 0x1C) addr = 0x0C;

        _paletteRam[addr] = data & 0x3F;
        return;
    }
}

constexpr Pixel PPU2C02_Memory::LookupSystemPalette(size_t index) {
    return _systemPalette[index & 0x3F]; // mask to 0-63
}

Pixel PPU2C02_Memory::GetColorFromPalette(uint8_t palette, uint8_t pixel) {
    uint16_t addr = 0x3F00;
    if (pixel != 0) {
        addr += ((uint16_t)palette << 2) + pixel;
    }
    return _systemPalette[PpuRead(addr) & 0x3F];
}