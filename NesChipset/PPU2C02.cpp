#include "PPU2C02.h"
#include <algorithm>
#include <core/Interfaces/CartridgeInterface.h>

PPU2C02::PPU2C02(Cartridge* cart)
    : _cart(nullptr), _pixelCallback(nullptr), _diagnosticCallback(nullptr)
{
    if (cart) {
        _cart = new CartridgeInterface(cart);
    }

    std::memset(_nametable0, 0, sizeof(_nametable0));
    std::memset(_nametable1, 0, sizeof(_nametable1));
    std::memset(_paletteRam, 0, sizeof(_paletteRam));
    std::memset(_patternTable0, 0, sizeof(_patternTable0));
    std::memset(_patternTable1, 0, sizeof(_patternTable1));

    InitializeSystemPalette();
    Reset();
}

PPU2C02::~PPU2C02() {
}

void PPU2C02::InitializeSystemPalette() {
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

void PPU2C02::Reset() {
    for (int i = 0; i < 64; i++) {
        OAM[i].Fill(0xFF);
    }

    _oamAddress = 0;
    _control.reg = 0;
    _mask.reg = 0;
    _status.reg = 0;
    _vramAddr.reg = 0;
    _tramAddr.reg = 0;
    _fineX = 0;
    _addressLatch = 0;
    _dataBuffer = 0;
    _scanline = 0;
    _cycle = 0;
    _oddFrame = false;
    _frameComplete = false;
    _nmiRequested = false;
    _scanlineTrigger = false;

    _bgNextTileId = 0;
    _bgNextTileAttrib = 0;
    _bgNextTileLsb = 0;
    _bgNextTileMsb = 0;
    _bgShifterPatternLo = 0;
    _bgShifterPatternHi = 0;
    _bgShifterAttribLo = 0;
    _bgShifterAttribHi = 0;

    _spriteCount = 0;
    _spriteZeroHitPossible = false;
    _spriteZeroBeingRendered = false;

    for (int i = 0; i < 8; i++) {
        _spriteScanline[i].Fill(0xFF);
        _spriteShifterLo[i] = 0;
        _spriteShifterHi[i] = 0;
    }
}

void PPU2C02::Log(const char* msg) {
    if (_diagnosticCallback) {
        _diagnosticCallback(msg);
    }
}

uint8_t PPU2C02::CpuRead(uint16_t addr, bool rdOnly) {
    uint8_t data = 0;

    if (rdOnly) {
        switch (addr) {
        case 0x0000: data = _control.reg; break;
        case 0x0001: data = _mask.reg; break;
        case 0x0002: data = _status.reg; break;
        }
    }
    else {
        switch (addr) {
        case 0x0002: // Status
            data = (_status.reg & 0xE0) | (_dataBuffer & 0x1F);
            _status.verticalBlank = false;
            _addressLatch = 0;
            break;

        case 0x0004: // OAM Data
            data = OAM[_oamAddress / 4].GetByteAt(_oamAddress);
            break;

        case 0x0007: // PPU Data
            data = _dataBuffer;
            _dataBuffer = PpuRead(_vramAddr.reg);
            if (_vramAddr.reg >= 0x3F00) data = _dataBuffer;
            _vramAddr.reg += (_control.incrementMode ? 32 : 1);
            break;
        }
    }

    return data;
}

void PPU2C02::CpuWrite(uint16_t addr, uint8_t data) {
    switch (addr) {
    case 0x0000: // Control
        _control.reg = data;
        _tramAddr.nametableX = _control.nametableX;
        _tramAddr.nametableY = _control.nametableY;
        break;

    case 0x0001: // Mask
        _mask.reg = data;
        break;

    case 0x0003: // OAM Address
        _oamAddress = data;
        break;

    case 0x0004: // OAM Data
        OAM[_oamAddress / 4].SetByteAt(_oamAddress, data);
        break;

    case 0x0005: // Scroll
        if (_addressLatch == 0) {
            _fineX = data & 0x07;
            _tramAddr.coarseX = data >> 3;
            _addressLatch = 1;
        }
        else {
            _tramAddr.fineY = data & 0x07;
            _tramAddr.coarseY = data >> 3;
            _addressLatch = 0;
        }
        break;

    case 0x0006: // Address
        if (_addressLatch == 0) {
            _tramAddr.reg = (uint16_t)((data & 0x3F) << 8) | (_tramAddr.reg & 0x00FF);
            _addressLatch = 1;
        }
        else {
            _tramAddr.reg = (_tramAddr.reg & 0xFF00) | data;
            _vramAddr = _tramAddr;
            _addressLatch = 0;
        }
        break;

    case 0x0007: // Data
        PpuWrite(_vramAddr.reg, data);
        _vramAddr.reg += (_control.incrementMode ? 32 : 1);
        break;
    }
}

uint8_t PPU2C02::PpuRead(uint16_t addr, bool rdOnly) {
    uint8_t data = 0;
    addr &= 0x3FFF;

    if (_cart->PpuRead(addr, data)) {
        // Cartridge handled the read
    }
    else if (addr <= 0x1FFF) {
        // Pattern tables
        data = (addr < 0x1000) ? _patternTable0[addr] : _patternTable1[addr & 0x0FFF];
    }
    else if (addr <= 0x3EFF) {
        // Nametables
        addr &= 0x0FFF;
        MirrorMode mirror = _cart->GetMirrorMode();

        if (mirror == MirrorMode::Vertical) {
            if (addr < 0x0400) {
                data = _nametable0[addr];
            }
            else if (addr < 0x0800) {
                data = _nametable1[addr & 0x03FF];
            }
            else if (addr < 0x0C00) {
                data = _nametable0[addr & 0x03FF];
            }
            else {
                data = _nametable1[addr & 0x03FF];
            }
        }
        else { // Horizontal
            if (addr < 0x0400) {
                data = _nametable0[addr];
            }
            else if (addr < 0x0800) {
                data = _nametable0[addr & 0x03FF];
            }
            else if (addr < 0x0C00) {
                data = _nametable1[addr & 0x03FF];
            }
            else {
                data = _nametable1[addr & 0x03FF];
            }
        }
    }
    else {
        // Palette RAM
        addr &= 0x1F;
        if (addr == 0x10) addr = 0x00;
        if (addr == 0x14) addr = 0x04;
        if (addr == 0x18) addr = 0x08;
        if (addr == 0x1C) addr = 0x0C;
        data = _paletteRam[addr] & (_mask.grayscale ? 0x30 : 0x3F);
    }

    return data;
}

void PPU2C02::PpuWrite(uint16_t addr, uint8_t data) {
    addr &= 0x3FFF;

    if (_cart->PpuWrite(addr, data)) {
        // Cartridge handled the write
    }
    else if (addr <= 0x1FFF) {
        // Pattern tables
        if (addr < 0x1000) {
            _patternTable0[addr] = data;
        }
        else {
            _patternTable1[addr & 0x0FFF] = data;
        }
    }
    else if (addr <= 0x3EFF) {
        // Nametables
        addr &= 0x0FFF;
        MirrorMode mirror = _cart->GetMirrorMode();

        if (mirror == MirrorMode::Vertical) {
            if (addr < 0x0400) {
                _nametable0[addr] = data;
            }
            else if (addr < 0x0800) {
                _nametable1[addr & 0x03FF] = data;
            }
            else if (addr < 0x0C00) {
                _nametable0[addr & 0x03FF] = data;
            }
            else {
                _nametable1[addr & 0x03FF] = data;
            }
        }
        else { // Horizontal
            if (addr < 0x0400) {
                _nametable0[addr] = data;
            }
            else if (addr < 0x0800) {
                _nametable0[addr & 0x03FF] = data;
            }
            else if (addr < 0x0C00) {
                _nametable1[addr & 0x03FF] = data;
            }
            else {
                _nametable1[addr & 0x03FF] = data;
            }
        }
    }
    else {
        // Palette RAM
        addr &= 0x1F;
        if (addr == 0x10) addr = 0x00;
        if (addr == 0x14) addr = 0x04;
        if (addr == 0x18) addr = 0x08;
        if (addr == 0x1C) addr = 0x0C;
        _paletteRam[addr] = data;
    }
}

Pixel PPU2C02::GetColorFromPalette(uint8_t palette, uint8_t pixel) {
    return _systemPalette[PpuRead(0x3F00 + ((uint16_t)palette << 2) + pixel) & 0x3F];
}

// Background rendering helpers
void PPU2C02::IncrementScrollX() {
    if (!_mask.renderBackground && !_mask.renderSprites) return;

    if (_vramAddr.coarseX == 31) {
        _vramAddr.coarseX = 0;
        _vramAddr.nametableX = !_vramAddr.nametableX;
    }
    else {
        _vramAddr.coarseX++;
    }
}

void PPU2C02::IncrementScrollY() {
    if (!_mask.renderBackground && !_mask.renderSprites) return;

    if (_vramAddr.fineY < 7) {
        _vramAddr.fineY++;
    }
    else {
        _vramAddr.fineY = 0;
        if (_vramAddr.coarseY == 29) {
            _vramAddr.coarseY = 0;
            _vramAddr.nametableY = !_vramAddr.nametableY;
        }
        else if (_vramAddr.coarseY == 31) {
            _vramAddr.coarseY = 0;
        }
        else {
            _vramAddr.coarseY++;
        }
    }
}

void PPU2C02::TransferAddressX() {
    if (!_mask.renderBackground && !_mask.renderSprites) return;
    _vramAddr.nametableX = _tramAddr.nametableX;
    _vramAddr.coarseX = _tramAddr.coarseX;
}

void PPU2C02::TransferAddressY() {
    if (!_mask.renderBackground && !_mask.renderSprites) return;
    _vramAddr.fineY = _tramAddr.fineY;
    _vramAddr.nametableY = _tramAddr.nametableY;
    _vramAddr.coarseY = _tramAddr.coarseY;
}

void PPU2C02::LoadBackgroundShifters() {
    _bgShifterPatternLo = (_bgShifterPatternLo & 0xFF00) | _bgNextTileLsb;
    _bgShifterPatternHi = (_bgShifterPatternHi & 0xFF00) | _bgNextTileMsb;
    _bgShifterAttribLo = (_bgShifterAttribLo & 0xFF00) | ((_bgNextTileAttrib & 1) ? 0xFF : 0);
    _bgShifterAttribHi = (_bgShifterAttribHi & 0xFF00) | ((_bgNextTileAttrib & 2) ? 0xFF : 0);
}

void PPU2C02::UpdateShifters() {
    if (_mask.renderBackground) {
        _bgShifterPatternLo <<= 1;
        _bgShifterPatternHi <<= 1;
        _bgShifterAttribLo <<= 1;
        _bgShifterAttribHi <<= 1;
    }

    if (_mask.renderSprites && _cycle >= 1 && _cycle < 258) {
        for (int i = 0; i < std::min((int)_spriteCount, 8); i++) {
            if (_spriteScanline[i].x > 0) {
                _spriteScanline[i].x--;
            }
            else {
                _spriteShifterLo[i] <<= 1;
                _spriteShifterHi[i] <<= 1;
            }
        }
    }
}

// Sprite rendering helpers
void PPU2C02::EvaluateSprites() {
    for (int i = 0; i < 8; i++) {
        _spriteScanline[i].Fill(0xFF);
        _spriteShifterLo[i] = 0;
        _spriteShifterHi[i] = 0;
    }

    _spriteCount = 0;
    _spriteZeroHitPossible = false;

    uint8_t entry = 0;
    while (entry < 64 && _spriteCount < 9) {
        int16_t diff = (_scanline + 1) - (int16_t)OAM[entry].y;
        int16_t height = _control.spriteSize ? 16 : 8;

        if (diff >= 0 && diff < height) {
            if (_spriteCount < 8) {
                if (entry == 0) _spriteZeroHitPossible = true;
                _spriteScanline[_spriteCount].CopyFrom(OAM[entry]);
                _spriteCount++;
            }
            else {
                _spriteCount++;
            }
        }
        entry++;
    }

    _status.spriteOverflow = (_spriteCount > 8);
}

void PPU2C02::LoadSpriteShifters() {
    for (int i = 0; i < std::min((int)_spriteCount, 8); i++) {
        uint8_t patternLo, patternHi;
        uint16_t addrLo, addrHi;

        int16_t spriteLine = (_scanline + 1) - (int16_t)_spriteScanline[i].y;

        if (!_control.spriteSize) {
            // 8x8 mode
            int16_t row = spriteLine;
            if (_spriteScanline[i].IsFlippedVertically()) row = 7 - row;

            addrLo = (_control.patternSprite ? 0x1000 : 0) |
                ((uint16_t)_spriteScanline[i].tileID << 4) |
                (uint16_t)row;
        }
        else {
            // 8x16 mode
            int16_t row = spriteLine;
            if (_spriteScanline[i].IsFlippedVertically()) row = 15 - row;

            uint16_t bank = (_spriteScanline[i].tileID & 1) << 12;
            uint16_t tile = _spriteScanline[i].tileID & 0xFE;
            if (row >= 8) {
                tile++;
                row -= 8;
            }

            addrLo = bank | (tile << 4) | row;
        }

        addrHi = addrLo + 8;
        patternLo = PpuRead(addrLo);
        patternHi = PpuRead(addrHi);

        if (_spriteScanline[i].IsFlippedHorizontally()) {
            patternLo = FlipByte(patternLo);
            patternHi = FlipByte(patternHi);
        }

        _spriteShifterLo[i] = patternLo;
        _spriteShifterHi[i] = patternHi;
    }
}

uint8_t PPU2C02::FlipByte(uint8_t b) {
    if (b == 0) return 0;
    b = ((b & 0xF0) >> 4) | ((b & 0x0F) << 4);
    b = ((b & 0xCC) >> 2) | ((b & 0x33) << 2);
    b = ((b & 0xAA) >> 1) | ((b & 0x55) << 1);
    return b;
}

void PPU2C02::Clock() {
    // Visible scanlines + pre-render
    if (_scanline >= -1 && _scanline < 240) {

        if (_scanline == 0 && _cycle == 0 && _oddFrame && (_mask.renderBackground || _mask.renderSprites)) {
            _cycle = 1;
        }

        if (_scanline == -1 && _cycle == 1) {
            _status.verticalBlank = false;
            _status.spriteOverflow = false;
            _status.spriteZeroHit = false;

            for (int i = 0; i < 8; i++) {
                _spriteShifterLo[i] = 0;
                _spriteShifterHi[i] = 0;
            }
        }

        if ((_cycle >= 2 && _cycle < 258) || (_cycle >= 321 && _cycle < 338)) {
            UpdateShifters();

            switch ((_cycle - 1) % 8) {
            case 0:
                LoadBackgroundShifters();
                _bgNextTileId = PpuRead(0x2000 | (_vramAddr.reg & 0x0FFF));
                break;

            case 2:
                _bgNextTileAttrib = PpuRead(0x23C0 |
                    ((_vramAddr.nametableY ? 1 : 0) << 11) |
                    ((_vramAddr.nametableX ? 1 : 0) << 10) |
                    ((_vramAddr.coarseY >> 2) << 3) |
                    (_vramAddr.coarseX >> 2));

                {
                    uint8_t shift = 0;
                    if ((_vramAddr.coarseY & 0x02) != 0) shift += 4;
                    if ((_vramAddr.coarseX & 0x02) != 0) shift += 2;
                    _bgNextTileAttrib = (_bgNextTileAttrib >> shift) & 0x03;
                }
                break;

            case 4:
                _bgNextTileLsb = PpuRead((_control.patternBackground ? 0x1000 : 0) |
                    ((uint16_t)_bgNextTileId << 4) |
                    _vramAddr.fineY);
                break;

            case 6:
                _bgNextTileMsb = PpuRead((_control.patternBackground ? 0x1000 : 0) |
                    ((uint16_t)_bgNextTileId << 4) |
                    _vramAddr.fineY + 8);
                break;

            case 7:
                IncrementScrollX();
                break;
            }
        }

        if (_cycle == 256) IncrementScrollY();

        if (_cycle == 257) {
            LoadBackgroundShifters();
            TransferAddressX();
        }

        if (_cycle == 338 || _cycle == 340) {
            _bgNextTileId = PpuRead(0x2000 | (_vramAddr.reg & 0x0FFF));
        }

        if (_scanline == -1 && _cycle >= 280 && _cycle < 305) {
            TransferAddressY();
        }

        if (_cycle == 257 && _scanline >= 0) {
            EvaluateSprites();
        }

        if (_cycle == 340) {
            LoadSpriteShifters();
        }
    }

    // Post-render scanline
    if (_scanline == 240) {
        // Idle
    }

    // VBlank
    if (_scanline >= 241 && _scanline < 261) {
        if (_scanline == 241 && _cycle == 1) {
            _status.verticalBlank = true;
            if (_control.enableNmi) _nmiRequested = true;
        }
    }

    // Render pixel
    uint8_t bgPixel = 0, bgPalette = 0;
    uint8_t fgPixel = 0, fgPalette = 0, fgPriority = 0;

    if (_mask.renderBackground) {
        if (_mask.renderBackgroundLeft || _cycle >= 9) {
            uint16_t mux = 0x8000 >> _fineX;
            bgPixel = (((_bgShifterPatternHi & mux) != 0 ? 1 : 0) << 1) | ((_bgShifterPatternLo & mux) != 0 ? 1 : 0);
            bgPalette = (((_bgShifterAttribHi & mux) != 0 ? 1 : 0) << 1) | ((_bgShifterAttribLo & mux) != 0 ? 1 : 0);
        }
    }

    if (_mask.renderSprites) {
        if (_mask.renderSpritesLeft || _cycle >= 9) {
            _spriteZeroBeingRendered = false;

            for (int i = 0; i < std::min((int)_spriteCount, 8); i++) {
                if (_spriteScanline[i].x == 0) {
                    fgPixel = (((_spriteShifterHi[i] & 0x80) != 0 ? 1 : 0) << 1) | ((_spriteShifterLo[i] & 0x80) != 0 ? 1 : 0);
                    fgPalette = (_spriteScanline[i].attributes & 0x03) + 4;
                    fgPriority = ((_spriteScanline[i].attributes & 0x20) == 0 ? 1 : 0);

                    if (fgPixel != 0) {
                        if (i == 0) _spriteZeroBeingRendered = true;
                        break;
                    }
                }
            }
        }
    }

    uint8_t pixel = 0, palette = 0;

    if (bgPixel == 0 && fgPixel == 0) {
        pixel = 0; palette = 0;
    }
    else if (bgPixel == 0 && fgPixel > 0) {
        pixel = fgPixel; palette = fgPalette;
    }
    else if (bgPixel > 0 && fgPixel == 0) {
        pixel = bgPixel; palette = bgPalette;
    }
    else if (bgPixel > 0 && fgPixel > 0) {
        if (fgPriority != 0) {
            pixel = fgPixel; palette = fgPalette;
        }
        else {
            pixel = bgPixel; palette = bgPalette;
        }

        if (_spriteZeroHitPossible && _spriteZeroBeingRendered) {
            if (_mask.renderBackground && _mask.renderSprites) {
                if (!(_mask.renderBackgroundLeft || _mask.renderSpritesLeft)) {
                    if (_cycle >= 9 && _cycle < 258) _status.spriteZeroHit = true;
                }
                else {
                    if (_cycle >= 1 && _cycle < 258) _status.spriteZeroHit = true;
                }
            }
        }
    }

    // Draw pixel
    if (_scanline >= 0 && _scanline < 240 && _cycle >= 1 && _cycle < 257) {
        if (_pixelCallback) {
            Pixel color = GetColorFromPalette(palette, pixel);
            _pixelCallback(_cycle - 1, _scanline, color.r, color.g, color.b);
        }
    }

    _cycle++;

    // Scanline counter for mappers (like MMC3)
    if (_mask.renderBackground || _mask.renderSprites) {
        if (_cycle == 260 && _scanline < 240) {
            MapperBase* mapper = _cart->GetMapper();
            if (mapper) {
                MapperScanlineCounter(mapper);
            }
        }
    }

    if (_cycle >= 341) {
        _cycle = 0;
        _scanline++;
        if (_scanline >= 261) {
            _scanline = -1;
            _frameComplete = true;
            _oddFrame = !_oddFrame;
        }
    }
}


void PPU2C02::GetPatternTable(uint8_t table, uint8_t palette, uint8_t* buffer) {
    if (!buffer) return;

    // 128x128 pattern table, 4 bytes per pixel (RGBA)
    for (int tileY = 0; tileY < 16; tileY++) {
        for (int tileX = 0; tileX < 16; tileX++) {
            uint16_t offset = (tileY * 256) + (tileX * 16);

            for (int row = 0; row < 8; row++) {
                uint8_t tileLsb = PpuRead((table * 0x1000) + offset + row);
                uint8_t tileMsb = PpuRead((table * 0x1000) + offset + row + 8);

                for (int col = 0; col < 8; col++) {
                    uint8_t pixel = ((tileMsb & 1) << 1) | (tileLsb & 1);
                    tileLsb >>= 1;
                    tileMsb >>= 1;

                    Pixel color = GetColorFromPalette(palette, pixel);

                    int x = tileX * 8 + (7 - col);
                    int y = tileY * 8 + row;
                    int index = (y * 128 + x) * 4;

                    buffer[index + 0] = color.r;
                    buffer[index + 1] = color.g;
                    buffer[index + 2] = color.b;
                    buffer[index + 3] = 255;
                }
            }
        }
    }
}

void PPU2C02::GetNameTable(uint8_t index, uint8_t* buffer) {
    if (!buffer || index > 1) return;

    // 256x240 nametable, 4 bytes per pixel (RGBA)
    uint8_t* nametable = (index == 0) ? _nametable0 : _nametable1;

    for (int y = 0; y < 30; y++) {
        for (int x = 0; x < 32; x++) {
            uint8_t tileId = nametable[y * 32 + x];
            uint8_t attrib = nametable[960 + (y / 4) * 8 + (x / 4)];

            uint8_t shift = 0;
            if ((y & 0x02) != 0) shift += 4;
            if ((x & 0x02) != 0) shift += 2;
            uint8_t palette = (attrib >> shift) & 0x03;

            // Draw tile
            for (int row = 0; row < 8; row++) {
                uint16_t addr = (_control.patternBackground ? 0x1000 : 0) + (tileId * 16) + row;
                uint8_t tileLsb = PpuRead(addr);
                uint8_t tileMsb = PpuRead(addr + 8);

                for (int col = 0; col < 8; col++) {
                    uint8_t pixel = ((tileMsb & 1) << 1) | (tileLsb & 1);
                    tileLsb >>= 1;
                    tileMsb >>= 1;

                    Pixel color = GetColorFromPalette(palette, pixel);

                    int px = x * 8 + (7 - col);
                    int py = y * 8 + row;
                    int index = (py * 256 + px) * 4;

                    buffer[index + 0] = color.r;
                    buffer[index + 1] = color.g;
                    buffer[index + 2] = color.b;
                    buffer[index + 3] = 255;
                }
            }
        }
    }
}

// Exported PPU functions
DLLEXPORT PPU2C02* CreatePPU(Cartridge* cart) {
    return new PPU2C02(cart);
}

DLLEXPORT void DestroyPPU(PPU2C02* ppu) {
    delete ppu;
}

DLLEXPORT void PPU_Reset(PPU2C02* ppu) {
    if (ppu) ppu->Reset();
}

DLLEXPORT void PPU_Clock(PPU2C02* ppu) {
    if (ppu) ppu->Clock();
}

DLLEXPORT uint8_t PPU_CpuRead(PPU2C02* ppu, uint16_t addr, bool rdOnly) {
    if (ppu) return ppu->CpuRead(addr, rdOnly);
    return 0;
}

DLLEXPORT void PPU_CpuWrite(PPU2C02* ppu, uint16_t addr, uint8_t data) {
    if (ppu) ppu->CpuWrite(addr, data);
}

DLLEXPORT bool PPU_IsFrameComplete(PPU2C02* ppu) {
    if (ppu) return ppu->IsFrameComplete();
    return false;
}

DLLEXPORT void PPU_SetFrameComplete(PPU2C02* ppu, bool value) {
    if (ppu) ppu->SetFrameComplete(value);
}

DLLEXPORT bool PPU_GetNmiRequested(PPU2C02* ppu) {
    if (ppu) return ppu->GetNmiRequested();
    return false;
}

DLLEXPORT void PPU_ClearNmiRequested(PPU2C02* ppu) {
    if (ppu) ppu->ClearNmiRequested();
}

DLLEXPORT void PPU_SetPixelCallback(PPU2C02* ppu, PixelCallback callback) {
    if (ppu) ppu->SetPixelCallback(callback);
}

DLLEXPORT void PPU_SetDiagnosticCallback(PPU2C02* ppu, DiagnosticCallback callback) {
    if (ppu) ppu->SetDiagnosticCallback(callback);
}

DLLEXPORT void PPU_GetPatternTable(PPU2C02* ppu, uint8_t table, uint8_t palette, uint8_t* buffer) {
    if (ppu) ppu->GetPatternTable(table, palette, buffer);
}

DLLEXPORT void PPU_GetOAMEntry(PPU2C02* ppu, uint8_t index, OAMEntry* entry) {
    if (ppu && entry && index < 64) {
        entry->CopyFrom(ppu->OAM[index]);
    }
}
