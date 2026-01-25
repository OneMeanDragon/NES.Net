#include "PPU2C02.Background.h"

void PPU2C02_Background::ResetBackground() {
    _bgNextTileId = 0;
    _bgNextTileAttrib = 0;
    _bgNextTileLsb = 0;
    _bgNextTileMsb = 0;
    _bgShifterPatternLo = 0;
    _bgShifterPatternHi = 0;
    _bgShifterAttribLo = 0;
    _bgShifterAttribHi = 0;
}

void PPU2C02_Background::IncrementScrollX(LoopyRegister& vramAddr, const PpuMaskRegister& mask) {
    if (!mask.renderBackground && !mask.renderSprites) return;

    if (vramAddr.coarseX == 31) {
        vramAddr.coarseX = 0;
        vramAddr.nametableX ^= 1;
    }
    else {
        vramAddr.coarseX++;
    }
}

void PPU2C02_Background::IncrementScrollY(LoopyRegister& vramAddr, const PpuMaskRegister& mask) {
    if (!mask.renderBackground && !mask.renderSprites) return;

    if (vramAddr.fineY < 7) {
        vramAddr.fineY++;
    }
    else {
        vramAddr.fineY = 0;
        if (vramAddr.coarseY == 29) {
            vramAddr.coarseY = 0;
            vramAddr.nametableY ^= 1;
        }
        else if (vramAddr.coarseY == 31) {
            vramAddr.coarseY = 0;
        }
        else {
            vramAddr.coarseY++;
        }
    }
}

void PPU2C02_Background::TransferAddressX(LoopyRegister& vramAddr, const LoopyRegister& tramAddr, const PpuMaskRegister& mask) {
    if (!mask.renderBackground && !mask.renderSprites) return;
    vramAddr.nametableX = tramAddr.nametableX;
    vramAddr.coarseX = tramAddr.coarseX;
}

void PPU2C02_Background::TransferAddressY(LoopyRegister& vramAddr, const LoopyRegister& tramAddr, const PpuMaskRegister& mask) {
    if (!mask.renderBackground && !mask.renderSprites) return;
    vramAddr.fineY = tramAddr.fineY;
    vramAddr.nametableY = tramAddr.nametableY;
    vramAddr.coarseY = tramAddr.coarseY;
}

void PPU2C02_Background::LoadBackgroundShifters() {
    _bgShifterPatternLo = (_bgShifterPatternLo & 0xFF00) | _bgNextTileLsb;
    _bgShifterPatternHi = (_bgShifterPatternHi & 0xFF00) | _bgNextTileMsb;

    uint8_t palLo = (_bgNextTileAttrib & 0x01) ? 0xFF : 0x00;
    uint8_t palHi = (_bgNextTileAttrib & 0x02) ? 0xFF : 0x00;

    _bgShifterAttribLo = (_bgShifterAttribLo & 0xFF00) | palLo;
    _bgShifterAttribHi = (_bgShifterAttribHi & 0xFF00) | palHi;
}

void PPU2C02_Background::UpdateBackgroundShifters(const PpuMaskRegister& mask, int16_t cycle) {
    if (mask.renderBackground && cycle >= 1 && cycle <= 256) {
        _bgShifterPatternLo <<= 1;
        _bgShifterPatternHi <<= 1;
        _bgShifterAttribLo <<= 1;
        _bgShifterAttribHi <<= 1;
    }
}

void PPU2C02_Background::FetchNametableByte(const LoopyRegister& vramAddr) {
    _bgNextTileId = PpuRead(0x2000 | (vramAddr.reg & 0x0FFF));
}

void PPU2C02_Background::FetchAttributeByte(const LoopyRegister& vramAddr) {
    uint16_t addr = 0x23C0 |
        (vramAddr.nametableY << 11) |
        (vramAddr.nametableX << 10) |
        ((vramAddr.coarseY >> 2) << 3) |
        (vramAddr.coarseX >> 2);

    uint8_t attr = PpuRead(addr);

    uint8_t shift = 0;
    if (vramAddr.coarseY & 0x02) shift += 4;
    if (vramAddr.coarseX & 0x02) shift += 2;

    _bgNextTileAttrib = (attr >> shift) & 0x03;
}

void PPU2C02_Background::FetchPatternLow(const LoopyRegister& vramAddr, const PpuControlRegister& control) {
    _bgNextTileLsb = PpuRead(
        (control.patternBackground ? 0x1000 : 0x0000) |
        (_bgNextTileId << 4) |
        vramAddr.fineY);
}

void PPU2C02_Background::FetchPatternHigh(const LoopyRegister& vramAddr, const PpuControlRegister& control) {
    _bgNextTileMsb = PpuRead(
        (control.patternBackground ? 0x1000 : 0x0000) |
        (_bgNextTileId << 4) |
        vramAddr.fineY + 8);
}

void PPU2C02_Background::GetBackgroundPixel(uint8_t fineX, uint8_t& pixel, uint8_t& palette) const {
    uint16_t bit = 0x8000 >> fineX;

    pixel = ((_bgShifterPatternHi & bit) ? 2 : 0) |
        ((_bgShifterPatternLo & bit) ? 1 : 0);

    palette = ((_bgShifterAttribHi & bit) ? 2 : 0) |
        ((_bgShifterAttribLo & bit) ? 1 : 0);
}