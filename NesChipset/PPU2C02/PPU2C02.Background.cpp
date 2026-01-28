#include "PPU2C02.Background.h"
#include <iostream>

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

// Always increment, ignore mask
void PPU2C02_Background::IncrementScrollX(LoopyRegister& v) {
    v.IncrementCoarseX();
}

void PPU2C02_Background::IncrementScrollY(LoopyRegister& v) {
    v.IncrementFineY();
}

// Always copy, ignore mask
void PPU2C02_Background::TransferAddressX(LoopyRegister& v, const LoopyRegister& t) {
    v.CopyHorizontalFrom(t);
}

void PPU2C02_Background::TransferAddressY(LoopyRegister& v, const LoopyRegister& t) {
    v.CopyVerticalFrom(t);
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
    // Only shift during visible pixel rendering (1-256)
    // Don't shift during prefetch (321-336) - just load
    if (mask.renderBackground && cycle >= 1 && cycle <= 256) {
        _bgShifterPatternLo <<= 1;
        _bgShifterPatternHi <<= 1;
        _bgShifterAttribLo <<= 1;
        _bgShifterAttribHi <<= 1;
    }
}

void PPU2C02_Background::FetchNametableByte(const LoopyRegister& vramAddr) {
    uint16_t addr = PPUADDR::NAMETABLE_BASE | (vramAddr.reg & PPUADDR::NAMETABLE_MASK);
    _bgNextTileId = PpuRead(addr);
}

void PPU2C02_Background::FetchAttributeByte(const LoopyRegister& vramAddr) {
    uint16_t v = vramAddr.reg & PPUADDR::NAMETABLE_MASK;
    uint16_t addr = PPUADDR::ATTR_TABLE_BASE_NT0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07);
    uint8_t attr = PpuRead(addr);

    uint8_t coarseX = vramAddr.CoarseX();
    uint8_t coarseY = vramAddr.CoarseY();
    uint8_t shift = (coarseX & 2) | ((coarseY & 2) << 1);

    _bgNextTileAttrib = (attr >> shift) & 0x03;
}

uint16_t PPU2C02_Background::GetPatternAddress(const LoopyRegister& vramAddr, const PpuControlRegister& control, bool highPlane) {
    uint16_t patternTableBase = control.patternBackground ? 0x1000 : 0x0000;
    uint16_t tileOffset = _bgNextTileId << 4;
    uint16_t row = vramAddr.FineY();
    uint16_t plane = highPlane ? 8 : 0;
    return patternTableBase | tileOffset | row | plane;
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