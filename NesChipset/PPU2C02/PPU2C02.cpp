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

    _scanline = SCANLINE_START;
    _cycle = CYCLE_START;
    _oddFrame = false;
    _frameComplete = false;
    _nmiRequested = false;
    _scanlineTrigger = false;
    _renderState = PPU::PpuState::PRERENDER;
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

void PPU2C02::PerformBackgroundFetch(int16_t cycle) {
    switch (cycle & 7) {
    case 1: FetchNametableByte(_vramAddr); break;
    case 3: FetchAttributeByte(_vramAddr); break;
    case 5: FetchPatternLow(_vramAddr, _control); break;
    case 7: FetchPatternHigh(_vramAddr, _control); break;
    case 0: // end of 8-cycle tile fetch
        LoadBackgroundShifters();
        IncrementScrollX(_vramAddr);
        //if (cycle != 256) IncrementScrollX(_vramAddr); // every tile except at end of scanline
        //if ((cycle >= 8 && cycle <= 256) || (cycle >= 328 && cycle <= 336)) IncrementScrollX(_vramAddr); // every tile except at end of scanline
        break;
    }
}

/* yes are pre-incrementing here */
void PPU2C02::AdvanceNext() {
    // Odd Frame Skip
    if (_scanline == -1 && _cycle == 339 && _oddFrame && (_mask.renderBackground || _mask.renderSprites)) {
        _scanline = 0;
        _cycle = 0;
        _frameComplete = false;
        UpdateRenderState();
        return;
    }
    // Advance the cycle
    if (++_cycle >= CYCLE_MAX) {
        _cycle = CYCLE_START;
        if (++_scanline >= SCANLINE_MAX) {
            _scanline = SCANLINE_START;
            _frameComplete = true;
            _oddFrame ^= 1;
        }
        UpdateRenderState();
    }
}

void PPU2C02::UpdateRenderState() {
    if (_scanline >= 0 && _scanline <= 239)
        _renderState = PPU::PpuState::VISIBLE;
    else if (_scanline == 240)
        _renderState = PPU::PpuState::POST;
    else if (_scanline >= 241 && _scanline <= 260)
        _renderState = PPU::PpuState::VBLANK_NMI;
    else
        _renderState = PPU::PpuState::PRERENDER;
}

void PPU2C02::ProcessCycle(int16_t scanline, int16_t cycle) {
    // --- Rendering state ---
    const bool renderingEnabled = _mask.renderBackground || _mask.renderSprites;

    // --- Cycle classification ---
    const bool visibleCycle = (cycle >= 1 && cycle <= 256);
    const bool prefetchCycle = (cycle >= 321 && cycle <= 336);
    const bool fetchCycle = visibleCycle || prefetchCycle;

    switch (_renderState) {
    case PPU::PpuState::PRERENDER:
    {
        // Clear flags at start of pre-render
        if (cycle == 1)
        {
            _status.verticalBlank = 0;
            _status.spriteZeroHit = 0;
            _status.spriteOverflow = 0;
        }

        // Background fetch pipeline
        if (renderingEnabled && fetchCycle)
        {
            UpdateBackgroundShifters(_mask, cycle);
            PerformBackgroundFetch(cycle);
        }

        // End-of-scanline scroll operations
        if (renderingEnabled)
        {
            // Increment vertical scroll
            if (cycle == 256)
            {
                IncrementScrollY(_vramAddr);
            }

            // Copy horizontal scroll & sprite eval
            if (cycle == 257)
            {
                TransferAddressX(_vramAddr, _tramAddr);
                EvaluateSprites(scanline + 1, _control, _status);
                LoadSpriteShifters(scanline + 1, _control);
            }

            // Copy vertical scroll during pre-render
            if (cycle >= 280 && cycle <= 304)
            {
                TransferAddressY(_vramAddr, _tramAddr);
            }
        }
        break;
    }

    case PPU::PpuState::VISIBLE:
    {
        // Background fetch pipeline
        if (renderingEnabled && fetchCycle)
        {
            UpdateBackgroundShifters(_mask, cycle);
            PerformBackgroundFetch(cycle);
        }

        // Pixel output
        if (visibleCycle)
        {
            //RenderPixel();
            //UpdateSpriteShifters(_mask, cycle);
            UpdateSpriteShifters(_mask, cycle);
            RenderPixel();
        }

        // End-of-scanline scroll & sprite operations
        if (renderingEnabled)
        {
            // Increment vertical scroll
            if (cycle == 256)
            {
                IncrementScrollY(_vramAddr);
            }

            // Copy horizontal scroll & sprite eval
            if (cycle == 257)
            {
                TransferAddressX(_vramAddr, _tramAddr);
                EvaluateSprites(scanline + 1, _control, _status);
                LoadSpriteShifters(scanline + 1, _control);
            }
        }
        break;
    }

    case PPU::PpuState::POST:
    {
        // Post-render scanline (240) - idle
        break;
    }

    case PPU::PpuState::VBLANK_NMI:
    {
        // Set VBlank flag and trigger NMI
        if (scanline == 241 && cycle == 1)
        {
            _status.verticalBlank = 1;
            if (_control.enableNmi) _nmiRequested = true;
        }
        break;
    }

    default:
        break;
    }

    // =========================================================
    // MAPPER SCANLINE HOOK (all states)
    // =========================================================
    //if (cycle == 260 && renderingEnabled && scanline >= 0 && scanline <= 239 && _cart)
    //{
    //    _cart->GetMapper().ScanlineCounter(scanline);
    //}
    if (cycle == 260 && (scanline >= 0 && scanline <= 239)) 
    {
        if (_mask.renderBackground || _mask.renderSprites) 
        {
            if (_cart) {
                _cart->GetMapper().ScanlineCounter(scanline);
            }
        }
    }
}

void PPU2C02::Clock() {

    ProcessCycle(_scanline, _cycle);
    AdvanceNext();
    
}

void PPU2C02::RenderPixel() {
//    if (_scanline == 100 && _cycle <= 10) {
//        printf("Cycle %d: fineX=%d shifterLo=%04X shifterHi=%04X\n",
//            _cycle, _fineX, _bgShifterPatternLo, _bgShifterPatternHi);
//    }

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
                    //    printf("SPR0 HIT @ scanline=%d cycle=%d\n", _scanline, _cycle);
                        _status.spriteZeroHit = 1;
                    }
                }
                else {
                //    printf("SPR0 HIT @ scanline=%d cycle=%d\n", _scanline, _cycle);
                    _status.spriteZeroHit = 1;
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

    uint8_t* nametable = ((index == 0) ? _nametable0 : _nametable1);

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