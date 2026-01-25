#include "PPU2C02.Sprites.h"
#include <algorithm>

PPU2C02_Sprites::PPU2C02_Sprites() {
    std::memset(OAM.raw, 0xFF, sizeof(OAM.raw)); // Initialize all OAM to 0xFF
    std::memset(_spriteScanline, 0xFF, sizeof(_spriteScanline));
    std::memset(_spriteShifterLo, 0, sizeof(_spriteShifterLo));
    std::memset(_spriteShifterHi, 0, sizeof(_spriteShifterHi));
}

void PPU2C02_Sprites::ResetSprites(bool coldstart) {
    if (coldstart) {
        std::fill(std::begin(OAM.raw), std::end(OAM.raw), 0xFF);
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
    // Optional debug print
    static int writeCount = 0;
    if (writeCount++ < 10) {
        printf("OAM Write: addr=%02X data=%02X\n", _oamAddress, data);
    }

    OAM.raw[_oamAddress] = data;
    _oamAddress = (_oamAddress + 1) & 0xFF; // Wrap around 0-255
}

void PPU2C02_Sprites::EvaluateSprites(int16_t scanline, const PpuControlRegister& control, PpuStatusRegister& status) {
    std::memset(_spriteScanline, 0xFF, sizeof(_spriteScanline));

    _spriteCount = 0;
    _spriteZeroHitPossible = false;

    int16_t height = control.spriteSize ? 16 : 8;
    uint8_t found = 0;

    status.spriteOverflow = false;
    for (uint8_t i = 0; i < 64; i++) {
        int16_t diff = scanline - (int16_t)OAM.entries[i].y;
        if (diff >= 0 && diff < height) {
            if (found < 8) {
                if (i == 0) _spriteZeroHitPossible = true;
                _spriteScanline[found++] = OAM.entries[i];
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
            // FIXED: Decrement X only when > 0
            // When X reaches 0, it stays at 0 and sprite becomes active
            if (_spriteScanline[i].x > 0) {
                _spriteScanline[i].x--;
            }
            else {
                // When X == 0, shift the pattern data
                // This outputs the sprite pixels
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

    // Check all sprites for the first visible one
    for (uint8_t i = 0; i < _spriteCount && i < 8; i++) {
        // Sprite is active when X == 0
        if (_spriteScanline[i].x == 0) {
            // Read MSB of shifter registers
            uint8_t pixelLo = (_spriteShifterLo[i] & 0x80) ? 1 : 0;
            uint8_t pixelHi = (_spriteShifterHi[i] & 0x80) ? 1 : 0;
            pixel = (pixelHi << 1) | pixelLo;

            if (pixel != 0) {
                palette = (_spriteScanline[i].attribute & 0x03) + 4;
                priority = (_spriteScanline[i].attribute & 0x20) == 0;
                if (i == 0) spriteZero = true;
                break;  // Use first non-transparent sprite
            }
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