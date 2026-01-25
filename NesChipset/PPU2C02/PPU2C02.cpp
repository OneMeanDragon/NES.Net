#include <cstdio>
#include <cstring>
#include <algorithm>

#include "PPU2C02.h"
#include "../CartridgeApi/MapperInterfaceAPI.h"
#include "../CartridgeApi/CartridgeInterfaceAPI.h"

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

/* Process cycle-by-cycle */
void PPU2C02::ProcessCycle(int16_t scanline, int16_t cycle) {
    // Scanline ranges
    bool preRenderLine = scanline == -1;
    bool visibleLine = scanline >= 0 && scanline <= 239;
    bool postRenderLine = scanline == 240;
    bool vblankLine = scanline >= 241 && scanline <= 260;

    // Cycle ranges
    bool visibleCycle = cycle >= 1 && cycle <= 256;
    bool prefetchCycle = cycle >= 321 && cycle <= 336;
    bool fetchCycle = visibleCycle || prefetchCycle;
    bool idleCycle = cycle == 0 || (cycle >= 257 && cycle <= 320) || cycle == 337 || cycle >= 338;

    // Rendering checks
    bool renderingEnabled = _mask.renderBackground || _mask.renderSprites;
    bool renderingLine = (preRenderLine || visibleLine) && renderingEnabled;

    // === PRE-RENDER AND VISIBLE SCANLINES ===
    if (preRenderLine || visibleLine) {
        // Odd frame skip
        if (scanline == 0 && cycle == 0 && _oddFrame && renderingEnabled) {
            return; // Skip cycle 0 on odd frames
        }

        // Clear flags at start of pre-render
        if (preRenderLine && cycle == 1) {
            _status.verticalBlank = false;
            _status.spriteZeroHit = false;
            _status.spriteOverflow = false;
        }

        // Background fetching
        if (renderingEnabled && fetchCycle) {
            UpdateBackgroundShifters(_mask, cycle);
            PerformBackgroundFetch(cycle);
        }

        // Pixel rendering (visible scanlines only)
        if (visibleLine && visibleCycle) {
            RenderPixel();
            UpdateSpriteShifters(_mask, cycle);
        }

        // End-of-scanline operations
        if (renderingEnabled) {
            if (cycle == 256) IncrementScrollY(_vramAddr, _mask);
            if (cycle == 257) {
                TransferAddressX(_vramAddr, _tramAddr, _mask);
                EvaluateSprites(scanline + 1, _control, _status);
                LoadSpriteShifters(scanline + 1, _control);
            }
            if (preRenderLine && cycle >= 280 && cycle <= 304) {
                TransferAddressY(_vramAddr, _tramAddr, _mask);
            }
        }
    }

    // === VBLANK ===
    if (scanline == 241 && cycle == 1) {
        _status.verticalBlank = true;
        if (_control.enableNmi) _nmiRequested = true;
    }

    // === MAPPER HOOKS ===
    if (renderingEnabled && cycle == 260 && scanline < 240 && _cart) {
        _cart->GetMapper().ScanlineCounter();
    }
}

void PPU2C02::PerformBackgroundFetch(int16_t cycle) {
    switch (cycle & 7) {
    case 1: FetchNametableByte(_vramAddr); break;
    case 3: FetchAttributeByte(_vramAddr); break;
    case 5: FetchPatternLow(_vramAddr, _control); break;
    case 7: FetchPatternHigh(_vramAddr, _control); break;
    case 0:
        LoadBackgroundShifters();
        if (cycle != 256) IncrementScrollX(_vramAddr, _mask);
        break;
    }
}

void PPU2C02::AdvanceNext() {
    _cycle++;
    if (_cycle >= CYCLE_MAX) {
        _cycle = 0;
        _scanline++;
        if (_scanline >= SCANLINE_MAX) {
            _scanline = SCANLINE_START;
            _frameComplete = true;
            _oddFrame = !_oddFrame;
        }
    }
}

void PPU2C02::Clock() {
    
    ProcessCycle(_scanline, _cycle);
    AdvanceNext();
    
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