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
    //uint16_t addr = 0x23C0 |
    //    (vramAddr.nametableY << 11) |
    //    (vramAddr.nametableX << 10) |
    //    ((vramAddr.coarseY >> 2) << 3) |
    //    (vramAddr.coarseX >> 2); is the following
    uint16_t addr = 0x23C0 | (vramAddr.reg & 0x0C00) | ((vramAddr.reg >> 4) & 0x38) | ((vramAddr.reg >> 2) & 0x07);

    uint8_t attr = PpuRead(addr);

    // Shift amount: 0 if top-left, 2 if top-right, 4 if bottom-left, 6 if bottom-right
    uint8_t shift = (vramAddr.coarseX & 2) | ((vramAddr.coarseY & 2) << 1);

    _bgNextTileAttrib = (attr >> shift) & 0x03;
}

uint16_t PPU2C02_Background::GetPatternAddress(const LoopyRegister& vramAddr, const PpuControlRegister& control, bool highPlane) {
    uint16_t patternTableBase = control.patternBackground ? 0x1000 : 0x0000;
    uint16_t tileOffset = _bgNextTileId << 4; // Tile ID × 16 bytes per tile
    uint16_t row = vramAddr.fineY;            // Which row within the tile (0-7)
    uint16_t plane = highPlane ? 8 : 0;

    uint16_t addr = patternTableBase | tileOffset | row | plane;

    // Debug: Uncomment to trace pattern fetches
    // printf("Pattern fetch: table=%04X tile=%02X row=%d plane=%s addr=%04X\n",
    //        patternTableBase, _bgNextTileId, row, highPlane ? "high" : "low", addr);

    return addr;
}

void PPU2C02_Background::FetchPatternLow(const LoopyRegister& vramAddr, const PpuControlRegister& control) {
    uint16_t addr = GetPatternAddress(vramAddr, control, false);
    _bgNextTileLsb = PpuRead(addr);
}

void PPU2C02_Background::FetchPatternHigh(const LoopyRegister& vramAddr, const PpuControlRegister& control) {
    uint16_t addr = GetPatternAddress(vramAddr, control, true);
    _bgNextTileMsb = PpuRead(addr);
}

void PPU2C02_Background::GetBackgroundPixel(uint8_t fineX, uint8_t& pixel, uint8_t& palette) const {
    uint16_t bit = 0x8000 >> fineX;

    pixel = ((_bgShifterPatternHi & bit) ? 2 : 0) |
        ((_bgShifterPatternLo & bit) ? 1 : 0);

    palette = ((_bgShifterAttribHi & bit) ? 2 : 0) |
        ((_bgShifterAttribLo & bit) ? 1 : 0);
}