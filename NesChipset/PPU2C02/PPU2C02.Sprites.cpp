#include "PPU2C02.Sprites.h"
#include <algorithm>

PPU2C02_Sprites::PPU2C02_Sprites() {
    std::memset(OAM, 0, sizeof(OAM));
    std::memset(_spriteScanline, 0xFF, sizeof(_spriteScanline));
    std::memset(_spriteShifterLo, 0, sizeof(_spriteShifterLo));
    std::memset(_spriteShifterHi, 0, sizeof(_spriteShifterHi));
}

void PPU2C02_Sprites::ResetSprites(bool coldstart) {
    if (coldstart) {
        std::fill(reinterpret_cast<uint8_t*>(OAM),
            reinterpret_cast<uint8_t*>(OAM) + 256, 0xFF);
    }

    _oamAddress = 0;
    _spriteCount = 0;
    _spriteZeroHitPossible = false;
    _spriteZeroBeingRendered = false;

    std::memset(_spriteScanline, 0xFF, sizeof(_spriteScanline));
    for (int i = 0; i < 8; i++) {
        _spriteShifterLo[i] = 0;
        _spriteShifterHi[i] = 0;
    }
}

void PPU2C02_Sprites::WriteOAM(uint8_t data) {
    reinterpret_cast<uint8_t*>(OAM)[_oamAddress] = data;
    _oamAddress++;
}

void PPU2C02_Sprites::EvaluateSprites(int16_t scanline, const PpuControlRegister& control, PpuStatusRegister& status) {
    std::memset(_spriteScanline, 0xFF, sizeof(_spriteScanline));
    for (int i = 0; i < 8; i++) {
        _spriteShifterLo[i] = 0;
        _spriteShifterHi[i] = 0;
    }

    _spriteCount = 0;
    _spriteZeroHitPossible = false;

    int16_t height = control.spriteSize ? 16 : 8;
    uint8_t found = 0;

    status.spriteOverflow = false;
    for (uint8_t i = 0; i < 64; i++) {
        // Sprite Y is the scanline where the TOP of the sprite appears
        // So sprite with Y=50 appears on scanlines 50-57 (for 8x8)
        int16_t diff = scanline - (int16_t)OAM[i].y;
        if (diff >= 0 && diff < height) {
            if (found < 8) {
                if (i == 0) _spriteZeroHitPossible = true;
                _spriteScanline[found++] = OAM[i];
            }
            else {
                status.spriteOverflow = true;
            }
        }
    }
    _spriteCount = found;
}

void PPU2C02_Sprites::LoadSpriteShifters(int16_t scanline, const PpuControlRegister& control) {
    for (int i = 0; i < std::min((int)_spriteCount, 8); i++) {
        uint8_t patternLo, patternHi;
        uint16_t addrLo, addrHi;

        int16_t spriteLine = scanline - (int16_t)_spriteScanline[i].y;

        if (!control.spriteSize) {
            // 8x8 mode
            int16_t row = spriteLine;
            if (_spriteScanline[i].attribute & 0x80) row = 7 - row;

            addrLo = (control.patternSprite ? 0x1000 : 0) |
                ((uint16_t)_spriteScanline[i].id << 4) |
                (uint16_t)row;
        }
        else {
            // 8x16 mode
            int16_t row = spriteLine;
            if (_spriteScanline[i].attribute & 0x80) row = 15 - row;

            uint16_t bank = (_spriteScanline[i].id & 1) << 12;
            uint16_t tile = _spriteScanline[i].id & 0xFE;
            if (row >= 8) {
                tile++;
                row -= 8;
            }

            addrLo = bank | (tile << 4) | row;
        }

        addrHi = addrLo + 8;
        patternLo = PpuRead(addrLo);
        patternHi = PpuRead(addrHi);

        if (_spriteScanline[i].attribute & 0x40) {
            patternLo = FlipByte(patternLo);
            patternHi = FlipByte(patternHi);
        }

        _spriteShifterLo[i] = patternLo;
        _spriteShifterHi[i] = patternHi;
    }
}

void PPU2C02_Sprites::UpdateSpriteShifters(const PpuMaskRegister& mask, int16_t cycle) {
    if (mask.renderSprites && cycle >= 1 && cycle < 258) {
        for (int i = 0; i < std::min((int)_spriteCount, 8); i++) {
            if (_spriteScanline[i].x > 0) {
                _spriteScanline[i].x--;
            }
            else {
                // Sprite is active, shift its pattern data
                _spriteShifterLo[i] <<= 1;
                _spriteShifterHi[i] <<= 1;
            }
        }
    }
}

void PPU2C02_Sprites::GetSpritePixel(uint8_t& pixel, uint8_t& palette, uint8_t& priority, bool& spriteZero) {
    pixel = 0;
    palette = 0;
    priority = 0;
    spriteZero = false;

    for (uint8_t i = 0; i < _spriteCount && i < 8; i++) {
        if (_spriteScanline[i].x == 0) {
            uint8_t pixelLo = (_spriteShifterLo[i] & 0x80) ? 1 : 0;
            uint8_t pixelHi = (_spriteShifterHi[i] & 0x80) ? 1 : 0;
            pixel = (pixelHi << 1) | pixelLo;

            if (pixel != 0) {
                palette = (_spriteScanline[i].attribute & 0x03) + 4;
                priority = !(_spriteScanline[i].attribute & 0x20);
                if (i == 0) spriteZero = true;
                break;
            }
        }
    }

    // Shift all active sprites after reading
    for (uint8_t i = 0; i < _spriteCount && i < 8; i++) {
        if (_spriteScanline[i].x == 0) {
            _spriteShifterLo[i] <<= 1;
            _spriteShifterHi[i] <<= 1;
        }
    }
}

constexpr uint8_t PPU2C02_Sprites::FlipByte(uint8_t b) noexcept {
    if (b == 0) return 0;
    b = ((b & 0xF0) >> 4) | ((b & 0x0F) << 4);
    b = ((b & 0xCC) >> 2) | ((b & 0x33) << 2);
    b = ((b & 0xAA) >> 1) | ((b & 0x55) << 1);
    return b;
}