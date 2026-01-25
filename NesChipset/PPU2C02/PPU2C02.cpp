#include "PPU2C02.h"
#include "../CartridgeApi/MapperInterfaceAPI.h"
#include "../CartridgeApi/CartridgeInterfaceAPI.h"
#include <cstdio>

PPU2C02::PPU2C02()
    : PPU2C02_Memory(),
    PPU2C02_Registers(),
    PPU2C02_Background(),
    PPU2C02_Sprites()
{
    Reset(true);
}

PPU2C02::~PPU2C02() {
}

void PPU2C02::SetCartridge(CartridgeInterfaceAPI* cart) {
    if (cart) {
        Log("Info: Valid CartridgeInterfaceAPI.");
        PPU2C02_Memory::SetCartridge(cart);
    }
    else {
        Log("Error: Invalid CartridgeInterfaceAPI.");
    }
}

void PPU2C02::Reset(bool coldstart) {
    PPU2C02_Memory::ResetMemory(coldstart);
    PPU2C02_Registers::ResetRegisters();
    PPU2C02_Background::ResetBackground();
    PPU2C02_Sprites::ResetSprites(coldstart);

    _scanline = 0;
    _cycle = 0;
    _oddFrame = false;
    _frameComplete = false;
    _nmiRequested = false;
    _scanlineTrigger = false;
}

// Forward CPU interface to Registers component
uint8_t PPU2C02::CpuRead(uint16_t addr, bool rdOnly) {
    return PPU2C02_Registers::CpuRead(addr, rdOnly);
}
void PPU2C02::CpuWrite(uint16_t addr, uint8_t data) {
    PPU2C02_Registers::CpuWrite(addr, data);
}

// Forward PPU bus interface to Memory component
uint8_t PPU2C02::PpuRead(uint16_t addr, bool rdOnly) {
    return PPU2C02_Memory::PpuRead(addr, rdOnly);
}
void PPU2C02::PpuWrite(uint16_t addr, uint8_t data) {
    PPU2C02_Memory::PpuWrite(addr, data);
}

void PPU2C02::Clock() {
    bool renderingEnabled = _mask.renderBackground || _mask.renderSprites;

    // --------------------------------------------------------
    // Pre-render & visible scanlines
    // --------------------------------------------------------
    if (_scanline >= -1 && _scanline <= 239) {
        // Odd frame skip
        if (_scanline == 0 && _cycle == 0 && _oddFrame && renderingEnabled)
            _cycle = 1;

        // Pre-render line initialization
        if (_scanline == -1 && _cycle == 1) {
            _status.verticalBlank = false;
            _status.spriteZeroHit = false;
            _status.spriteOverflow = false;

            // Preload first background tile
            FetchNametableByte(_vramAddr);
            FetchAttributeByte(_vramAddr);
            FetchPatternLow(_vramAddr, _control);
            FetchPatternHigh(_vramAddr, _control);
            LoadBackgroundShifters();
        }

        // Background fetch pipeline (1-256, 321-336)
        if (renderingEnabled &&
            ((_cycle >= 1 && _cycle <= 256) ||
                (_cycle >= 321 && _cycle <= 336))) {

            UpdateBackgroundShifters(_mask, _cycle);

            switch ((_cycle - 1) % 8) {
            case 0:
                LoadBackgroundShifters();
                FetchNametableByte(_vramAddr);
                break;
            case 2:
                FetchAttributeByte(_vramAddr);
                break;
            case 4:
                FetchPatternLow(_vramAddr, _control);
                break;
            case 6:
                FetchPatternHigh(_vramAddr, _control);
                break;
            case 7:
                IncrementScrollX(_vramAddr, _mask);
                break;
            }
        }

        // End of scanline operations
        if (renderingEnabled) {
            if (_cycle == 256) {
                IncrementScrollY(_vramAddr, _mask);
            }
            if (_cycle == 257) {
                TransferAddressX(_vramAddr, _tramAddr, _mask);
                EvaluateSprites(_scanline, _control, _status);
                LoadSpriteShifters(_scanline, _control);
            }
            if (_scanline == -1 && _cycle >= 280 && _cycle <= 304) {
                TransferAddressY(_vramAddr, _tramAddr, _mask);
            }
        }
    }

    // VBlank
    if (_scanline == 241 && _cycle == 1) {
        _status.verticalBlank = true;
        if (_control.enableNmi) _nmiRequested = true;
    }

    // --------------------------------------------------------
    // Pixel rendering (BEFORE updating shifters)
    // --------------------------------------------------------
    if (_scanline >= 0 && _scanline < 240 &&
        _cycle >= 1 && _cycle <= 256) {
        RenderPixel();
    }

    // Update sprite shifters AFTER rendering
    if (_scanline >= -1 && _scanline <= 239) {
        UpdateSpriteShifters(_mask, _cycle);
    }

    // --------------------------------------------------------
    // Advance cycle
    // --------------------------------------------------------
    _cycle++;
    if (_cycle >= 341) {
        _cycle = 0;
        _scanline++;
        if (_scanline >= 261) {
            _scanline = -1;
            _frameComplete = true;
            _oddFrame = !_oddFrame;
        }
    }

    // Scanline hooks for mappers
    if (renderingEnabled&& _cycle == 260 && _scanline < 240 && _cart) {
        _cart->GetMapper().ScanlineCounter();
    }
}

void PPU2C02::RenderPixel() {
    bool bgLeftAllowed = _mask.renderBackgroundLeft || _cycle > 8;
    bool sprLeftAllowed = _mask.renderSpritesLeft || _cycle > 8;

    uint8_t bgPixel = 0, bgPalette = 0;
    uint8_t fgPixel = 0, fgPalette = 0, fgPriority = 0;
    bool spriteZero = false;

    // Get background pixel
    if (_mask.renderBackground && bgLeftAllowed) {
        GetBackgroundPixel(_fineX, bgPixel, bgPalette);
    }

    // Get sprite pixel
    if (_mask.renderSprites && sprLeftAllowed) {
        GetSpritePixel(fgPixel, fgPalette, fgPriority, spriteZero);
        _spriteZeroBeingRendered = spriteZero;
    }
    else {
        _spriteZeroBeingRendered = false;
    }

    // Compose final pixel
    uint8_t pixel = 0, palette = 0;

    if (bgPixel == 0 && fgPixel == 0) {
        pixel = 0;
        palette = 0;
    }
    else if (bgPixel == 0 && fgPixel > 0) {
        pixel = fgPixel;
        palette = fgPalette;
    }
    else if (bgPixel > 0 && fgPixel == 0) {
        pixel = bgPixel;
        palette = bgPalette;
    }
    else {
        // Both visible - check priority
        if (fgPriority) {
            pixel = fgPixel;
            palette = fgPalette;
        }
        else {
            pixel = bgPixel;
            palette = bgPalette;
        }

        // Sprite 0 hit detection
        if (_spriteZeroHitPossible && _spriteZeroBeingRendered && _cycle >= 1) {
            if (_mask.renderBackground && _mask.renderSprites) {
                if (!(_mask.renderBackgroundLeft && _mask.renderSpritesLeft)) {
                    if (_cycle >= 9) {
                        _status.spriteZeroHit = true;
                    }
                }
                else {
                    _status.spriteZeroHit = true;
                }
            }
        }
    }

    // Send pixel to display
    if (_pixelCallback) {
        Pixel c = GetColorFromPalette(palette, pixel);
        _pixelCallback(_cycle - 1, _scanline, c.r, c.g, c.b);
    }
}

void PPU2C02::Log(const char* msg) {
    if (_diagnosticCallback) {
        _diagnosticCallback(msg);
    }
}

void PPU2C02::GetPatternTable(uint8_t table, uint8_t palette, uint8_t* buffer) {
    if (!buffer) return;

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

    uint8_t* nametable = (index == 0) ? _nametable0 : _nametable1;

    for (int y = 0; y < 30; y++) {
        for (int x = 0; x < 32; x++) {
            uint8_t tileId = nametable[y * 32 + x];
            uint8_t attrib = nametable[960 + (y / 4) * 8 + (x / 4)];

            uint8_t shift = 0;
            if ((y & 0x02) != 0) shift += 4;
            if ((x & 0x02) != 0) shift += 2;
            uint8_t palette = (attrib >> shift) & 0x03;

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