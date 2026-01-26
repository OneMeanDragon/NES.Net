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

void PPU2C02_Background::IncrementScrollX(LoopyRegister& vramAddr, const PpuMaskRegister& mask) {
    if (!mask.renderBackground && !mask.renderSprites) return;
    vramAddr.IncrementCoarseX();
}

void PPU2C02_Background::IncrementScrollY(LoopyRegister& vramAddr, const PpuMaskRegister& mask) {
    if (!mask.renderBackground && !mask.renderSprites) return;
    vramAddr.IncrementFineY();
}

void PPU2C02_Background::TransferAddressX(LoopyRegister& vramAddr, const LoopyRegister& tramAddr, const PpuMaskRegister& mask) {
    if (!mask.renderBackground && !mask.renderSprites) return;

    // DEBUG: Check if tramAddr is corrupt BEFORE transfer
    //if (tramAddr.reg > 0x3FFF) {
    //    printf("TransferAddressX: tramAddr is ALREADY corrupt! tramAddr=%04X\n", tramAddr.reg);
    //}

    vramAddr.SetNametableX(tramAddr.GetNametableX());
    vramAddr.SetCoarseX(tramAddr.GetCoarseX());

    // DEBUG: Check result
    //if (vramAddr.reg > 0x3FFF) {
    //    printf("TransferAddressX: vramAddr corrupted AFTER transfer! vramAddr=%04X\n", vramAddr.reg);
    //}
}

void PPU2C02_Background::TransferAddressY(LoopyRegister& vramAddr, const LoopyRegister& tramAddr, const PpuMaskRegister& mask) {
    if (!mask.renderBackground && !mask.renderSprites) return;
    vramAddr.SetFineY(tramAddr.GetFineY());
    vramAddr.SetNametableY(tramAddr.GetNametableY());
    vramAddr.SetCoarseY(tramAddr.GetCoarseY());
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
    //if (vramAddr.reg >= 0x3F00) {
    //    printf("ERROR: FetchNametable with palette addr=%04X!\n", vramAddr.reg);
    //}
    uint16_t addr = 0x2000 | (vramAddr.reg & 0x0FFF);
    _bgNextTileId = PpuRead(addr);
}

void PPU2C02_Background::FetchAttributeByte(const LoopyRegister& vramAddr) {
    uint16_t v = vramAddr.reg & 0x0FFF;
    uint16_t addr = 0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07);
    uint8_t attr = PpuRead(addr);

    uint8_t coarseX = vramAddr.GetCoarseX();
    uint8_t coarseY = vramAddr.GetCoarseY();
    uint8_t shift = (coarseX & 2) | ((coarseY & 2) << 1);

    _bgNextTileAttrib = (attr >> shift) & 0x03;
}

uint16_t PPU2C02_Background::GetPatternAddress(const LoopyRegister& vramAddr, const PpuControlRegister& control, bool highPlane) {
    uint16_t patternTableBase = control.patternBackground ? 0x1000 : 0x0000;
    uint16_t tileOffset = _bgNextTileId << 4;
    uint16_t row = vramAddr.GetFineY();
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