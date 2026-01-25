#include "PPU2C02.h"
#include <algorithm>

#include "CartridgeApi/MapperInterfaceAPI.h"
#include "CartridgeApi/CartridgeInterfaceAPI.h"

constexpr uint16_t nametableAddr(uint16_t addressVRAM) {
    return 0x2000 | (addressVRAM & 0x0FFF);
}
constexpr uint16_t attributeAddr(uint16_t addressVRAM) {
    return 0x23C0
        | (addressVRAM & (VRAM_NAMETABLEX | VRAM_NAMETABLEY))  // 0000001111100000 //VRAM_COARSEY
        | ((addressVRAM & VRAM_COARSEX) >> 2)                  //     001110000000
        | ((addressVRAM & 0x0380) >> 4);                       // (VRAM_COARSEY >> 2) << 3)
}
constexpr uint16_t bgPatternAddr(uint8_t control_reg, uint16_t bgNametableLatch, uint16_t addressVRAM)
{
    return ((control_reg & CTRL_BACKGROUNDPATTERN) << 8)
        | (bgNametableLatch << 4)
        | ((addressVRAM & VRAM_FINEY) >> 12);
}

PPU2C02::PPU2C02()
    : _cart(nullptr), _pixelCallback(nullptr), _diagnosticCallback(nullptr)
{
    std::memset(_nametable0, 0, sizeof(_nametable0));
    std::memset(_nametable1, 0, sizeof(_nametable1));
    std::memset(_paletteRam, 0, sizeof(_paletteRam));

    InitializeSystemPalette();
    Reset(true);
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

void PPU2C02::Reset(bool coldstart) {
    if (coldstart) {
        // Power-on behavior
        for (int i = 0; i < 64; i++) OAM[i].Fill(0xFF);
        for (int i = 0; i < 32; i++) _paletteRam[i] = 0x00;
    }

    _oamAddress = 0;
    _control.reg = 0;
    _mask.reg = 0;
    _status.reg = 0xA0;
    _vramAddr = 0;
    _tramAddr = 0;
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
        case 0x0003: data = _oamAddress; break;
        case 0x0005: data = 0; break; // Write-only
        case 0x0006: data = 0; break; // Write-only
        }
    }
    else {
        switch (addr) {
        case PPUCTRL:
        case PPUMASK:
        case OAMADDR:
        case PPUSCROLL:
        case PPUADDR:
            break;

        case PPUSTATUS: // PPUSTATUS
            // Top 3 bits from status, bottom 5 bits from data buffer (PPU open bus)
            data = (_status.reg & 0xE0) | (_dataBuffer & 0x1F);
            _status.verticalBlank = false;
            // updatenmi?
            _addressLatch = 0;
            // secondwrite?
            break;

        case OAMDATA: // OAMDATA
            // Reading during rendering returns different values
            // For simplicity, we read the current OAM address
            data = OAM[_oamAddress / 4].GetByteAt(_oamAddress);
            break;

        case PPUDATA: // PPUDATA
            data = _dataBuffer;
            _dataBuffer = PpuRead(_vramAddr);

            // Palette reads are not buffered
            if (_vramAddr >= 0x3F00) {
                data = _dataBuffer;
            }

            _vramAddr += (_control.incrementMode ? 32 : 1);
            break;
        }
    }

    return data;
}

void PPU2C02::CpuWrite(uint16_t addr, uint8_t data) {
    switch (addr) {
    case PPUCTRL: // PPUCTRL
        _control.reg = data;
        //
        _tramAddr &= ~(VRAM_NAMETABLEX | VRAM_NAMETABLEY);
        _tramAddr |= (data & 0x03) << 10;
        break;

    case PPUMASK: // PPUMASK
        _mask.reg = data;
        break;

    case OAMADDR: // OAMADDR
        // OAM Corruption?
        _oamAddress = data;
        break;

    case OAMDATA: // OAMDATA
        OAM[_oamAddress / 4].SetByteAt(_oamAddress, data);
        _oamAddress++;
        break;

    case PPUSCROLL: // PPUSCROLL
        //if (_addressLatch == 0) {
        //    _tramAddr &= ~VRAM_COARSEX;
        //    _tramAddr |= data >> 3;
        //    _fineX = data & 0x7;
        //}
        //else {
        //    _tramAddr &= ~(VRAM_COARSEY | VRAM_FINEY);
        //    _tramAddr |= static_cast<uint16_t>(data & 0x07) << 12;  // Fine Y: bits 0-2 -> 12-14
        //    _tramAddr |= static_cast<uint16_t>(data >> 3) << 5;     // Coarse Y: bits 3-7 -> 5-9
        //}
        if (_addressLatch == 0) {
            _tramAddr &= ~VRAM_COARSEX;
            _tramAddr |= data >> 3;
            _fineX = data & 0x7;
        }
        else {
            _tramAddr &= ~(VRAM_COARSEY | VRAM_FINEY);
            uint16_t fineY = (data & 0x07) << 12;
            uint16_t coarseY = (data & 0xF8) << 2;
            _tramAddr |= fineY | coarseY;

            // DEBUG: Print what we just set
//            printf("PPUSCROLL Y: data=%d, fineY=%d, coarseY=%d, tramAddr=%04X\n",
//                data, (fineY >> 12), (coarseY >> 5), _tramAddr);
        }
        _addressLatch ^= 1;
        break;

    case PPUADDR: // PPUADDR
        if (_addressLatch == 0) {
            _tramAddr &= 0xFF;
            _tramAddr |= static_cast<uint16_t>(data & 0x3f) << 8;
        }
        else {
            _tramAddr &= 0xFF00; //0x7F00
            _tramAddr |= data;
            _vramAddr = _tramAddr;
        }
        _addressLatch ^= 1;
        break;

    case PPUDATA: // PPUDATA
        PpuWrite(_vramAddr, data);
        _vramAddr += (_control.incrementMode ? 32 : 1);
        break;
    }
}

uint8_t PPU2C02::PpuRead(uint16_t addr, bool rdOnly) {
    uint8_t data = 0;
    addr &= 0x3FFF;

    // Pattern tables ($0000-$1FFF) - handled by cartridge
    if (addr <= 0x1FFF) {
        if (!_cart->PpuRead(addr, &data)) {
            // If cartridge doesn't handle it, return open bus
            data = 0;
        }
    }
    // Nametables ($2000-$3EFF)
    else if (addr <= 0x3EFF) {
        addr &= 0x0FFF;

        MirrorMode mirror = _cart->GetMirrorMode();

        if (mirror == MirrorMode::Vertical) {
            // Vertical: $2000=$2800, $2400=$2C00
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
        else if (mirror == MirrorMode::Horizontal) {
            // Horizontal: $2000=$2400, $2800=$2C00
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
        else if (mirror == MirrorMode::OneScreenLo) {
            // All nametables map to first
            data = _nametable0[addr & 0x03FF];
        }
        else if (mirror == MirrorMode::OneScreenHi) {
            // All nametables map to second
            data = _nametable1[addr & 0x03FF];
        }
        else {
            // Four-screen or other mapper-controlled mirroring
            // Let the cartridge handle it
            if (!_cart->PpuRead(0x2000 | addr, &data)) {
                // Default to nametable 0 if not handled
                data = _nametable0[addr & 0x03FF];
            }
        }
    }
    // Palette RAM ($3F00-$3FFF)
    else {
        addr &= 0x1F;

        // Mirror backdrop color addresses
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

    // Pattern tables ($0000-$1FFF) - handled by cartridge (CHR RAM if writable)
    if (addr <= 0x1FFF) {
        _cart->PpuWrite(addr, data);
    }
    // Nametables ($2000-$3EFF)
    else if (addr <= 0x3EFF) {
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
        else if (mirror == MirrorMode::Horizontal) {
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
        else if (mirror == MirrorMode::OneScreenLo) {
            _nametable0[addr & 0x03FF] = data;
        }
        else if (mirror == MirrorMode::OneScreenHi) {
            _nametable1[addr & 0x03FF] = data;
        }
        else {
            // Four-screen or mapper-controlled
            _cart->PpuWrite(0x2000 | addr, data);
        }
    }
    // Palette RAM ($3F00-$3FFF)
    else {
        addr &= 0x1F;

        // Mirror backdrop color addresses
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

    if ((_vramAddr & VRAM_COARSEX) == VRAM_COARSEX) {
        _vramAddr &= ~VRAM_COARSEX;
        _vramAddr ^= VRAM_NAMETABLEX;
    }
    else { _vramAddr++; }
}

void PPU2C02::IncrementScrollY() {
    if (!_mask.renderBackground && !_mask.renderSprites) return;

    if ((_vramAddr & VRAM_FINEY) == VRAM_FINEY) {
        // Fine Y at max (7), wrap to 0 and increment coarse Y
        _vramAddr &= ~VRAM_FINEY;  // Clear fine Y bits

        uint16_t coarseY = (_vramAddr & VRAM_COARSEY) >> 5;

        if (coarseY == 29) {
            // Last visible row, wrap to top of OTHER nametable
            _vramAddr &= ~VRAM_COARSEY;  // Reset coarse Y to 0
            _vramAddr ^= VRAM_NAMETABLEY; // Toggle vertical nametable
        }
        else if (coarseY == 31) {
            // In the off-screen area, just wrap
            _vramAddr &= ~VRAM_COARSEY;
        }
        else {
            // Normal case: increment coarse Y
            _vramAddr += 0x0020;  // Add 1 to coarse Y (bit 5)
        }
    }
    else {
        // Increment fine Y
        _vramAddr += 0x1000;  // Add 1 to fine Y (bit 12)
    }
}

//void PPU2C02::TransferAddressX() {
//    if (!_mask.renderBackground && !_mask.renderSprites) return;
//    _vramAddr &= ~(VRAM_COARSEX | VRAM_NAMETABLEX);
//    _vramAddr |= (_tramAddr & (VRAM_COARSEX | VRAM_NAMETABLEX));
//}
//
//void PPU2C02::TransferAddressY() {
//    if (!_mask.renderBackground && !_mask.renderSprites) return;
//    _vramAddr &= ~(VRAM_COARSEY | VRAM_NAMETABLEY | VRAM_FINEY);
//    _vramAddr |= (_tramAddr & (VRAM_COARSEY | VRAM_NAMETABLEY | VRAM_FINEY));
//}
void PPU2C02::TransferAddressX() {
    if (!_mask.renderBackground && !_mask.renderSprites) return;
    _vramAddr = (_vramAddr & ~(VRAM_COARSEX | VRAM_NAMETABLEX))
        | (_tramAddr & (VRAM_COARSEX | VRAM_NAMETABLEX));
}

void PPU2C02::TransferAddressY() {
    if (!_mask.renderBackground && !_mask.renderSprites) return;
    //_vramAddr = (_vramAddr & ~(VRAM_COARSEY | VRAM_NAMETABLEY | VRAM_FINEY))
    //    | (_tramAddr & (VRAM_COARSEY | VRAM_NAMETABLEY | VRAM_FINEY));

    uint16_t old_vram = _vramAddr;
    _vramAddr = (_vramAddr & ~(VRAM_COARSEY | VRAM_NAMETABLEY | VRAM_FINEY))
        | (_tramAddr & (VRAM_COARSEY | VRAM_NAMETABLEY | VRAM_FINEY));

    printf("TransferY: tramAddr=%04X, old_vramAddr=%04X, new_vramAddr=%04X\n",
        _tramAddr, old_vram, _vramAddr);
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
        int16_t diff = ((int16_t)_scanline) - (int16_t)OAM[entry].y;
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

        int16_t spriteLine = ((int16_t)_scanline) - (int16_t)_spriteScanline[i].y;

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
    // Visible scanlines (0-239) + pre-render scanline (-1)
    if (_scanline >= -1 && _scanline < 240) {

        // Skip cycle 0 of scanline 0 on odd frames (if rendering enabled)
        if (_scanline == 0 && _cycle == 0 && _oddFrame && (_mask.renderBackground || _mask.renderSprites)) {
            _cycle = 1;
        }

        // Start of pre-render scanline - clear flags
        if (_scanline == -1 && _cycle == 1) {
            _status.verticalBlank = false;
            _status.spriteOverflow = false;
            _status.spriteZeroHit = false;

            for (int i = 0; i < 8; i++) {
                _spriteShifterLo[i] = 0;
                _spriteShifterHi[i] = 0;
            }
        }

        // Background tile fetching and rendering
        if ((_cycle >= 2 && _cycle < 258) || (_cycle >= 321 && _cycle < 338)) {
            UpdateShifters();

            switch ((_cycle - 1) % 8) {
            case 0:
                LoadBackgroundShifters();
                _bgNextTileId = PpuRead(nametableAddr(_vramAddr));
                break;

            case 2:
                // Fetch attribute byte
                _bgNextTileAttrib = PpuRead(attributeAddr(_vramAddr));

                // TEMPORARY: Force palette 0
                //_bgNextTileAttrib = 0;
                // Extract the 2-bit palette from the attribute byte
                {
                    uint8_t shift = 0;
                    if (((_vramAddr & VRAM_COARSEY) & 0x40) != 0) shift += 4;
                    if (((_vramAddr & VRAM_COARSEX) & 0x02) != 0) shift += 2;
                    _bgNextTileAttrib = (_bgNextTileAttrib >> shift) & 0x03;
                }
                break;

            case 4:
                // Fetch tile LSB
                _bgNextTileLsb = PpuRead(bgPatternAddr(_control.reg, _bgNextTileId, _vramAddr));
                break;

            case 6:
                // Fetch tile MSB
                _bgNextTileMsb = PpuRead(bgPatternAddr(_control.reg, _bgNextTileId, _vramAddr) + 0x8);
                break;

            case 7:
                IncrementScrollX();
                break;
            }
        }

        // End of visible scanline - increment Y
        if (_cycle == 256) {
            IncrementScrollY();
        }

        // Copy horizontal position from t to v
        if (_cycle == 257) {
            LoadBackgroundShifters();
            TransferAddressX();
        }

        // Unused nametable fetches (but mappers might use these cycles)
        if (_cycle == 338 || _cycle == 340) {
            _bgNextTileId = PpuRead(nametableAddr(_vramAddr));
        }

        // Pre-render scanline: copy vertical position from t to v
        if (_scanline == -1 && _cycle >= 280 && _cycle < 305) {
            TransferAddressY();
        }

        // Sprite evaluation for next scanline
        if (_cycle == 257 && _scanline >= 0) {
            EvaluateSprites();
        }

        // Load sprite shifters for current scanline
        if (_cycle == 340) {
            LoadSpriteShifters();
        }
    }

    // Post-render scanline (240) - idle
    if (_scanline == 240) {
        // Do nothing
    }

    // VBlank scanlines (241-260)
    if (_scanline >= 241 && _scanline < 261) {
        if (_scanline == 241 && _cycle == 1) {
            _status.verticalBlank = true;
            if (_control.enableNmi) {
                _nmiRequested = true;
            }
        }
    }

    // ============================================
    // PIXEL RENDERING
    // ============================================
    uint8_t bgPixel = 0, bgPalette = 0;
    uint8_t fgPixel = 0, fgPalette = 0, fgPriority = 0;

    // Get background pixel
    if (_mask.renderBackground) {
        // Left 8 pixels clipping
        if (_mask.renderBackgroundLeft || _cycle >= 9) {
            uint16_t mux = 0x8000 >> _fineX;
            uint8_t pixelLo = (_bgShifterPatternLo & mux) != 0 ? 1 : 0;
            uint8_t pixelHi = (_bgShifterPatternHi & mux) != 0 ? 1 : 0;
            bgPixel = (pixelHi << 1) | pixelLo;

            uint8_t palLo = (_bgShifterAttribLo & mux) != 0 ? 1 : 0;
            uint8_t palHi = (_bgShifterAttribHi & mux) != 0 ? 1 : 0;
            bgPalette = (palHi << 1) | palLo;
        }
    }

    // Get sprite pixel
    if (_mask.renderSprites) {
        // Left 8 pixels clipping
        if (_mask.renderSpritesLeft || _cycle >= 9) {
            _spriteZeroBeingRendered = false;

            for (int i = 0; i < std::min((int)_spriteCount, 8); i++) {
                if (_spriteScanline[i].x == 0) {
                    uint8_t pixelLo = (_spriteShifterLo[i] & 0x80) != 0 ? 1 : 0;
                    uint8_t pixelHi = (_spriteShifterHi[i] & 0x80) != 0 ? 1 : 0;
                    fgPixel = (pixelHi << 1) | pixelLo;
                    fgPalette = (_spriteScanline[i].attributes & 0x03) + 4;
                    fgPriority = (_spriteScanline[i].attributes & 0x20) == 0 ? 1 : 0;

                    if (fgPixel != 0) {
                        if (i == 0) _spriteZeroBeingRendered = true;
                        break;
                    }
                }
            }
        }
    }

    // Combine background and sprite pixels
    uint8_t pixel = 0, palette = 0;

    if (bgPixel == 0 && fgPixel == 0) {
        // Both transparent - use backdrop color
        pixel = 0;
        palette = 0;
    }
    else if (bgPixel == 0 && fgPixel > 0) {
        // Background transparent, sprite opaque
        pixel = fgPixel;
        palette = fgPalette;
    }
    else if (bgPixel > 0 && fgPixel == 0) {
        // Background opaque, sprite transparent
        pixel = bgPixel;
        palette = bgPalette;
    }
    else {
        // Both opaque - priority decides
        if (fgPriority) {
            pixel = fgPixel;
            palette = fgPalette;
        }
        else {
            pixel = bgPixel;
            palette = bgPalette;
        }

        // Sprite zero hit detection
        if (_spriteZeroHitPossible && _spriteZeroBeingRendered) {
            if (_mask.renderBackground && _mask.renderSprites) {
                // Check if both left edge clipping flags are off
                if (!(_mask.renderBackgroundLeft || _mask.renderSpritesLeft)) {
                    // Hit only happens in cycles 9-255
                    if (_cycle >= 9 && _cycle < 258) {
                        _status.spriteZeroHit = true;
                    }
                }
                else {
                    // Hit can happen in cycles 1-255
                    if (_cycle >= 1 && _cycle < 258) {
                        _status.spriteZeroHit = true;
                    }
                }
            }
        }
    }

    // Draw the pixel
    if (_scanline >= 0 && _scanline < 240 && _cycle >= 1 && _cycle <= 256) {
        if (_pixelCallback) {
            Pixel color = GetColorFromPalette(palette, pixel);
            _pixelCallback(_cycle - 1, _scanline, color.r, color.g, color.b);
        }
    }

    // Advance cycle and scanline counters
    _cycle++;

    // Notify mappers of scanline events (important for MMC3/Mapper 4)
    if (_mask.renderBackground || _mask.renderSprites) {
        if (_cycle == 260 && _scanline < 240) {
            MapperInterfaceAPI mapper = _cart->GetMapper();
            mapper.ScanlineCounter();
        }
    }

    // End of scanline
    if (_cycle >= 341) {
        _cycle = 0;
        _scanline++;

        // End of frame
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
            uint8_t pal = (attrib >> shift) & 0x03;

            // Draw tile
            for (int row = 0; row < 8; row++) {
                uint16_t addr = (_control.patternBackground ? 0x1000 : 0) + (tileId * 16) + row;
                uint8_t tileLsb = PpuRead(addr);
                uint8_t tileMsb = PpuRead(addr + 8);

                for (int col = 0; col < 8; col++) {
                    uint8_t pixel = ((tileMsb & 1) << 1) | (tileLsb & 1);
                    tileLsb >>= 1;
                    tileMsb >>= 1;

                    Pixel color = GetColorFromPalette(pal, pixel);

                    int px = x * 8 + (7 - col);
                    int py = y * 8 + row;
                    int bufIdx = (py * 256 + px) * 4;

                    buffer[bufIdx + 0] = color.r;
                    buffer[bufIdx + 1] = color.g;
                    buffer[bufIdx + 2] = color.b;
                    buffer[bufIdx + 3] = 255;
                }
            }
        }
    }
}

void PPU2C02::SetCartridge(CartridgeInterfaceAPI* cart) {
    if (cart) {
        Log("Info: Valid CartridgeInterfaceAPI.");
        _cart = cart;
    }
    else {
        Log("Error: Invalid CartridgeInterfaceAPI.");
    }
}

// ============================================
// EXPORTED PPU API FUNCTIONS
// ============================================

DLLEXPORT PPU2C02* CreatePPU() {
    return new PPU2C02();
}

DLLEXPORT void DestroyPPU(PPU2C02* ppu) {
    delete ppu;
}

DLLEXPORT void PPU_Reset(PPU2C02* ppu, bool coldstart) {
    if (ppu) ppu->Reset(coldstart);
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

DLLEXPORT void PPU_SetDiagnosticCallback(PPU2C02* ppu, DiagnosticLogCallback callback) {
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

DLLEXPORT void PPU_SetOAMEntry(PPU2C02* ppu, uint8_t index, OAMEntry* entry) {
    if (ppu && entry && index < 64) {
        ppu->OAM[index].CopyFrom(*entry);
    }
}

DLLEXPORT uint8_t PPU_GetOAMByte(PPU2C02* ppu, uint8_t oamAddr) {
    if (!ppu) return 0xFF;
    uint8_t index = oamAddr / 4;
    if (index >= 64) return 0xFF;
    return ppu->OAM[index].GetByteAt(oamAddr);
}

DLLEXPORT void PPU_SetOAMByte(PPU2C02* ppu, uint8_t oamAddr, uint8_t data) {
    if (!ppu) return;
    uint8_t index = oamAddr / 4;
    if (index >= 64) return;
    ppu->OAM[index].SetByteAt(oamAddr, data);
}

DLLEXPORT void PPU_GetColorFromPalette(PPU2C02* ppu, uint8_t palette, uint8_t pixel, uint8_t* r, uint8_t* g, uint8_t* b) {
    if (ppu && r && g && b) {
        Pixel color = ppu->GetColorFromPalette(palette, pixel);
        *r = color.r;
        *g = color.g;
        *b = color.b;
    }
}