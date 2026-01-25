#include "PPU2C02.h"
#include <algorithm>

#include "../CartridgeApi/MapperInterfaceAPI.h"
#include "../CartridgeApi/CartridgeInterfaceAPI.h"

PPU2C02::PPU2C02()
    : _cart(nullptr), _pixelCallback(nullptr), _diagnosticCallback(nullptr)
{
    std::memset(_nametable0, 0, sizeof(_nametable0));
    std::memset(_nametable1, 0, sizeof(_nametable1));
    std::memset(_nametable2, 0, sizeof(_nametable2));
    std::memset(_nametable3, 0, sizeof(_nametable3));
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

    // 0x30-0x3F MIRROR 0x20-0x2F (not separate colors!)
    //for (int i = 0; i < 16; i++) {
    //    _systemPalette[0x30 + i] = _systemPalette[0x20 + i];
    //}

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
    if (coldstart) { // we just inserted a cartridge
        // Power-on behavior
        std::fill(reinterpret_cast<uint8_t*>(OAM), reinterpret_cast<uint8_t*>(OAM) + 256, 0xFF); // Palette either random or all zeros
        std::fill(reinterpret_cast<uint8_t*>(_paletteRam), reinterpret_cast<uint8_t*>(_paletteRam) + 32, 0xFF); // Palette either random or all zeros
    } // warm reset preserves the above    

    _oamAddress = 0;
    _control.reg = 0;
    _mask.reg = 0;
    _status.reg = 0xA0; // 0
    _vramAddr.reg = 0x2000;
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

    std::memset(_spriteScanline, 0xFF, sizeof(_spriteScanline));
    for (int i = 0; i < 8; i++) {
        _spriteShifterLo[i] = 0;
        _spriteShifterHi[i] = 0;
    }
}

void PPU2C02::Log(const char* msg) {
    if (_diagnosticCallback) {
        _diagnosticCallback(msg);
    }
}

//void PPU2C02::OamDma(uint8_t* page) {
//    // Copy 256 bytes from CPU memory page into OAM
//    std::memcpy(OAM, page, 256);
//}

uint8_t PPU2C02::CpuRead(uint16_t addr, bool rdOnly) {
    uint8_t data = 0x00;

    if (rdOnly) {
        // Read-only mode for debugger/tools - no side effects
        switch (addr) {
        case 0x0000: data = _control.reg; break;
        case 0x0001: data = _mask.reg; break;
        case 0x0002: data = _status.reg; break;
        case 0x0003: data = _oamAddress; break;
        case 0x0004:
            data = reinterpret_cast<uint8_t*>(OAM)[_oamAddress];
            break;
        case 0x0007: {
            data = _dataBuffer;

            //if (addr >= 0x8000) {  // Reading from ROM
            //static bool debugRead = false;
            //if (!debugRead) {
            //    printf("CPU read only from ROM $%04X = %02X\n", addr, data);
            //    printf("XXX read only from ROM $%04X = %02X\n", addr, _dataBuffer);
            //    debugRead = true;
            //}
            //}

            break;
        }
        }
    }
    else {
        // Normal read mode - has side effects
        switch (addr) {
        case 0x0000: // PPUCTRL   ($2000) - Write-only
        case 0x0001: // PPUMASK   ($2001) - Write-only
        case 0x0003: // OAMADDR   ($2003) - Write-only
        case 0x0005: // PPUSCROLL ($2005) - Write-only
        case 0x0006: // PPUADDR   ($2006) - Write-only
            // All write-only registers return open bus
            data = _openBus;
            break;

        case 0x0002: // PPUSTATUS ($2002) - Read-only
            // Bits 7-5 from status, bits 4-0 from open bus
            data = (_status.reg & 0xE0) | (_openBus & 0x1F);
            _status.verticalBlank = false;
            _addressLatch = 0;
            break;

        case 0x0004: // OAMDATA ($2004) - Read/Write
            data = reinterpret_cast<uint8_t*>(OAM)[_oamAddress];
            break;

        case 0x0007: {// PPUDATA ($2007) - Read/Write

            if (addr >= 0x3F00)
            {
                // Palette read returns real data
                data = PpuRead(addr);

                // BUT buffer is updated with mirrored nametable byte
                _dataBuffer = PpuRead(addr & 0x2FFF);
            }
            else
            {
                data = _dataBuffer;
                _dataBuffer = PpuRead(addr);
            }

            _vramAddr.reg += (_control.incrementMode ? 32 : 1);
            break;
        }
        default:
            data = _openBus;
            break;
        }
    }

    // Update open bus with the value being returned
    _openBus = data;

    return data;
}

void PPU2C02::CpuWrite(uint16_t addr, uint8_t data) {
    _openBus = data;


    switch (addr) {
    case 0x0000: // PPUCTRL ($2000)
        _control.reg = data;
        // Update the temporary VRAM address nametable bits
        _tramAddr.nametableX = _control.nametableX;
        _tramAddr.nametableY = _control.nametableY;
        break;

    case 0x0001: { // PPUMASK ($2001)
        _mask.reg = data;
        //printf("PPUMASK = %02X\n", data);
        break;
    }

    case 0x0002: // PPUSTATUS ($2002)
        // Read-only register, writes are ignored
        break;

    case 0x0003: // OAMADDR ($2003)
        _oamAddress = data;
        break;

    case 0x0004: // OAMDATA ($2004)
        // Write to OAM at current address
        reinterpret_cast<uint8_t*>(OAM)[_oamAddress] = data;
        _oamAddress++;  // Auto-increment after write
        break;

    case 0x0005: // PPUSCROLL ($2005)
        if (_addressLatch == 0) {
            // First write: X scroll
            _fineX = data & 0x07;
            _tramAddr.coarseX = data >> 3;
            _addressLatch = 1;
        }
        else {
            // Second write: Y scroll
            _tramAddr.fineY = data & 0x07;
            _tramAddr.coarseY = data >> 3;
            _addressLatch = 0;
        }
        break;

    case 0x0006: // PPUADDR ($2006)
        if (_addressLatch == 0) {
            // First write: high byte (only bits 0-5 are valid, bit 6-7 ignored)
            _tramAddr.reg = (uint16_t)((data & 0x3F) << 8) | (_tramAddr.reg & 0x00FF);
            _addressLatch = 1;
        }
        else {
            // Second write: low byte
            _tramAddr.reg = (_tramAddr.reg & 0xFF00) | data;
            _vramAddr = _tramAddr;  // Transfer temp to actual address
            _addressLatch = 0;
        }
        break;

    case 0x0007: {// PPUDATA ($2007)
        //static bool debugWritten = false;
        //if (!debugWritten && _vramAddr.reg >= 0x2000 && _vramAddr.reg <= 0x3EFF) {
        //    printf("Writing to VRAM $%04X = %02X\n", _vramAddr.reg, data);
        //    debugWritten = true;  // Only print first write
        //}
        PpuWrite(_vramAddr.reg, data);
        // Increment VRAM address by 1 or 32 depending on PPUCTRL bit 2
        _vramAddr.reg += (_control.incrementMode ? 32 : 1);
        break;
    }

    default:
        // Invalid address - should never reach here if bus is correct
        printf("Mapper CPU write %04X = %02X\n", addr, data);
        break;
    }
}

uint8_t PPU2C02::ReadNametable(uint16_t addr)
{
    addr &= 0x0FFF; // $2000-$2FFF mirrored to $000-$FFF
    MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;

    switch (mirror) {
    case MirrorMode::Horizontal:
        if (addr < 0x0800) return _nametable0[addr & 0x03FF];
        else return _nametable1[addr & 0x03FF];
    case MirrorMode::Vertical:
        if (addr < 0x0400) return _nametable0[addr];
        else if (addr < 0x0800) return _nametable1[addr - 0x0400];
        else if (addr < 0x0C00) return _nametable0[addr - 0x0800];
        else return _nametable1[addr - 0x0C00];
    default:
        return _nametable0[addr & 0x03FF];
    }
}

void PPU2C02::WriteNametable(uint16_t addr, uint8_t data)
{
    addr &= 0x0FFF; // $2000-$2FFF mirrored to $000-$FFF
    MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;

    switch (mirror) {
    case MirrorMode::Horizontal:
        if (addr < 0x0800) _nametable0[addr & 0x03FF] = data;
        else _nametable1[addr & 0x03FF] = data;
        break;
    case MirrorMode::Vertical:
        if (addr < 0x0400) _nametable0[addr] = data;
        else if (addr < 0x0800) _nametable1[addr - 0x0400] = data;
        else if (addr < 0x0C00) _nametable0[addr - 0x0800] = data;
        else _nametable1[addr - 0x0C00] = data;
        break;
    default:
        _nametable0[addr & 0x03FF] = data;
        break;
    }
}

uint8_t PPU2C02::PpuRead(uint16_t addr, bool rdOnly) {
    uint8_t data = 0;
    addr &= 0x3FFF;

    // DEBUG: Pattern table reads
    //if (addr <= 0x1FFF) {
    //    static int count = 0;
    //    if (count++ < 20) {
    //        printf("PpuRead pattern: $%04X = %02X\n", addr, data);
    //    }
    //}

    if (addr <= 0x1FFF) {
        // Pattern tables ($0000-$1FFF) - ALWAYS from cartridge
        if (_cart && _cart->PpuRead(addr, &data)) {
            return data;
        }
        // No cartridge loaded - return open bus
        return 0x00;
    }
    else if (addr >= 0x2000 && addr <= 0x3EFF) {
        // Nametables ($2000-$3EFF, mirrored every $1000 bytes)
        addr &= 0x0FFF;  // Mirror down to $0000-$0FFF range

        MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;
        //printf("CART->MIRRORMODE=%d\n", (uint8_t)mirror);

        switch (mirror) {
        case MirrorMode::Vertical:
            // $2000=$2800, $2400=$2C00
            data = ReadNametable(addr & 0x03FF);
            break;

        case MirrorMode::Horizontal:
            data = ReadNametable(addr & 0x03FF);
            break;

        case MirrorMode::OneScreenLo:
            // All map to nametable 0
            data = _nametable0[addr & 0x03FF];
            break;

        case MirrorMode::OneScreenHi:
            // All map to nametable 1
            data = _nametable1[addr & 0x03FF];
            break;

        case MirrorMode::FourScreen:
            // Need 4 nametables for this (cart has extra VRAM)
            // You'd need _nametable2 and _nametable3, or handle in cartridge
            // For now, fall back to vertical
            if (addr < 0x0800) {
                data = _nametable0[addr & 0x03FF];
            }
            else {
                data = _nametable1[addr & 0x03FF];
            }
            break;

        default:
            data = 0x00;
            break;
        }
    }
    else if (addr >= 0x3F00 && addr <= 0x3FFF) {
        // Palette RAM ($3F00-$3F1F, mirrored to $3FFF)
        addr &= 0x1F;

        // Mirror background color addresses
        if (addr == 0x10) addr = 0x00;
        if (addr == 0x14) addr = 0x04;
        if (addr == 0x18) addr = 0x08;
        if (addr == 0x1C) addr = 0x0C;

        data = _paletteRam[addr] & 0x3F;

        // Apply grayscale mask on READ
        //if (_mask.grayscale) {
        //    data &= 0x30;
        //}
        //else {
        //    data = _paletteRam[addr];
        //    data &= 0x3F;
        //}
    }

    return data;
}

void PPU2C02::PpuWrite(uint16_t addr, uint8_t data) {
    addr &= 0x3FFF;

    if (addr <= 0x1FFF) {
        // Pattern tables - count CHR writes
        static int chrWriteCount = 0;
        chrWriteCount++;

        if (_cart) {
            _cart->PpuWrite(addr, data);
        }
        return;
    }
    else if (addr >= 0x2000 && addr <= 0x3EFF) {
        // Nametables
        addr &= 0x0FFF;
        MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;

        switch (mirror) {
        case MirrorMode::Vertical:
            WriteNametable(addr & 0x03FF, data);
            break;
        case MirrorMode::Horizontal:
            WriteNametable(addr & 0x03FF, data);
            break;
        case MirrorMode::OneScreenLo:
            _nametable0[addr & 0x03FF] = data;
            break;
        case MirrorMode::OneScreenHi:
            _nametable1[addr & 0x03FF] = data;
            break;
        case MirrorMode::FourScreen:
            if (addr < 0x0800) {
                _nametable0[addr & 0x03FF] = data;
            }
            else {
                _nametable1[addr & 0x03FF] = data;
            }
            break;
        }
    }
    else if (addr >= 0x3F00 && addr <= 0x3FFF) {
        // Palette RAM
        addr &= 0x1F;
        if (addr == 0x10) addr = 0x00;
        if (addr == 0x14) addr = 0x04;
        if (addr == 0x18) addr = 0x08;
        if (addr == 0x1C) addr = 0x0C;
        _paletteRam[addr] = data & 0x3F;
    }
}

Pixel PPU2C02::GetColorFromPalette(uint8_t palette, uint8_t pixel) {
    // Pixel 0 of ANY palette reads from universal background at $3F00
    uint16_t addr = 0x3F00;
    if (pixel != 0) {
        addr += ((uint16_t)palette << 2) + pixel;
    }
    return _systemPalette[PpuRead(addr) & 0x3F];
}

// Background rendering helpers
void PPU2C02::IncrementScrollX() {
    if (!_mask.renderBackground && !_mask.renderSprites) return;

    if (_vramAddr.coarseX == 31) {
        _vramAddr.coarseX = 0;
        _vramAddr.nametableX ^= 1; //!_vramAddr.nametableX;
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
            _vramAddr.nametableY ^= 1; //!_vramAddr.nametableY;
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

    uint8_t palLo = (_bgNextTileAttrib & 0x01) ? 0xFF : 0x00;
    uint8_t palHi = (_bgNextTileAttrib & 0x02) ? 0xFF : 0x00;

    _bgShifterAttribLo = (_bgShifterAttribLo & 0xFF00) | palLo;
    _bgShifterAttribHi = (_bgShifterAttribHi & 0xFF00) | palHi;
}

void PPU2C02::UpdateShifters()
{
    // Background shifters only shift during visible pixels
    if (_mask.renderBackground && _cycle >= 1 && _cycle <= 256)
    {
        _bgShifterPatternLo <<= 1;
        _bgShifterPatternHi <<= 1;
        _bgShifterAttribLo <<= 1;
        _bgShifterAttribHi <<= 1;
    }

    // Sprite shifters unchanged
    if (_mask.renderSprites && _cycle >= 1 && _cycle < 258)
    {
        for (int i = 0; i < std::min((int)_spriteCount, 8); i++)
        {
            if (_spriteScanline[i].x > 0)
                _spriteScanline[i].x--;
            else
            {
                _spriteShifterLo[i] <<= 1;
                _spriteShifterHi[i] <<= 1;
            }
        }
    }
}

// Sprite rendering helpers
void PPU2C02::EvaluateSprites() {
    std::memset(_spriteScanline, 0xFF, sizeof(_spriteScanline));
    for (int i = 0; i < 8; i++) {
        _spriteShifterLo[i] = 0;
        _spriteShifterHi[i] = 0;
    }

    _spriteCount = 0;
    _spriteZeroHitPossible = false;

    int16_t height = _control.spriteSize ? 16 : 8;
    uint8_t found = 0;

    _status.spriteOverflow = false;
    for (uint8_t i = 0; i < 64; i++) {
        int16_t diff = _scanline - OAM[i].y;
        if (diff >= 0 && diff < height) {
            if (found < 8) {
                if (i == 0) _spriteZeroHitPossible = true;
                _spriteScanline[found++] = OAM[i];
            }
            else {
                _status.spriteOverflow = true;
            }
        }
    }
    _spriteCount = found;
}

void PPU2C02::LoadSpriteShifters() {
    for (int i = 0; i < std::min((int)_spriteCount, 8); i++) {
        uint8_t patternLo, patternHi;
        uint16_t addrLo, addrHi;

        // FIXED: Use current scanline, not +1
        int16_t spriteLine = _scanline - (int16_t)_spriteScanline[i].y;

        if (!_control.spriteSize) {
            // 8x8 mode
            int16_t row = spriteLine;
            if (_spriteScanline[i].attribute & 0x80) row = 7 - row;

            addrLo = (_control.patternSprite ? 0x1000 : 0) |
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

constexpr uint8_t PPU2C02::FlipByte(uint8_t b) noexcept {
    if (b == 0) return 0;
    b = ((b & 0xF0) >> 4) | ((b & 0x0F) << 4);
    b = ((b & 0xCC) >> 2) | ((b & 0x33) << 2);
    b = ((b & 0xAA) >> 1) | ((b & 0x55) << 1);
    return b;
}

void PPU2C02::Clock()
{
    bool renderingEnabled = _mask.renderBackground || _mask.renderSprites;

    // --------------------------------------------------------
    // Pre-render & visible scanlines
    // --------------------------------------------------------
    if (_scanline >= -1 && _scanline <= 239)
    {
        // Odd frame skip
        if (_scanline == 0 && _cycle == 0 && _oddFrame && renderingEnabled)
            _cycle = 1;

        // Pre-render line
        if (_scanline == -1 && _cycle == 1)
        {
            // Preload first background tile shifters
            _bgNextTileId = PpuRead(0x2000 | (_vramAddr.reg & 0x0FFF));
            _bgNextTileAttrib = 0;
            _bgNextTileLsb = PpuRead((_control.patternBackground ? 0x1000 : 0x0000) | (_bgNextTileId << 4) | _vramAddr.fineY);
            _bgNextTileMsb = PpuRead((_control.patternBackground ? 0x1000 : 0x0000) | (_bgNextTileId << 4) | _vramAddr.fineY + 8);
            LoadBackgroundShifters();
        }

        // Background fetch pipeline (1-256, 321-336)
        if (renderingEnabled &&
            ((_cycle >= 1 && _cycle <= 256) ||
                (_cycle >= 321 && _cycle <= 336)))
        {
            UpdateShifters(); // **shift BEFORE using shifters**

            switch ((_cycle - 1) % 8)
            {
            case 0: // Load shifters for next tile
                LoadBackgroundShifters();
                _bgNextTileId = PpuRead(0x2000 | (_vramAddr.reg & 0x0FFF));
                break;

            case 2: // Attribute byte
            {
                uint16_t addr =
                    0x23C0 |
                    (_vramAddr.nametableY << 11) |
                    (_vramAddr.nametableX << 10) |
                    ((_vramAddr.coarseY >> 2) << 3) |
                    (_vramAddr.coarseX >> 2);

                uint8_t attr = PpuRead(addr);

                uint8_t shift = 0;
                if (_vramAddr.coarseY & 0x02) shift += 4;
                if (_vramAddr.coarseX & 0x02) shift += 2;

                _bgNextTileAttrib = (attr >> shift) & 0x03;
                break;
            }

            case 4: // Pattern low byte
                _bgNextTileLsb =
                    PpuRead(
                        (_control.patternBackground ? 0x1000 : 0x0000) |
                        (_bgNextTileId << 4) |
                        _vramAddr.fineY);
                break;

            case 6: // Pattern high byte
                _bgNextTileMsb =
                    PpuRead(
                        (_control.patternBackground ? 0x1000 : 0x0000) |
                        (_bgNextTileId << 4) |
                        _vramAddr.fineY + 8);
                break;

            case 7: // Increment coarse X
                IncrementScrollX();
                break;
            }
        }

        // End of scanline
        if (renderingEnabled)
        {
            if (_cycle == 256) IncrementScrollY();
            if (_cycle == 257)
            {
                TransferAddressX();
                EvaluateSprites();
                LoadSpriteShifters();
            }
            if (_scanline == -1 && _cycle >= 280 && _cycle <= 304)
                TransferAddressY();
        }
    }

    // VBlank
    if (_scanline == 241 && _cycle == 1)
    {
        _status.verticalBlank = true;
        if (_control.enableNmi) _nmiRequested = true;
    }

    // --------------------------------------------------------
    // Pixel rendering
    // --------------------------------------------------------
    if (_scanline >= 0 && _scanline < 240 &&
        _cycle >= 1 && _cycle <= 256)
    {
        bool bgLeftAllowed = _mask.renderBackgroundLeft || _cycle > 8;
        bool sprLeftAllowed = _mask.renderSpritesLeft || _cycle > 8;
        if (_cycle < 9) {
            bgLeftAllowed = false;
            sprLeftAllowed = false;
        }
        uint8_t bgPixel = 0, bgPalette = 0;
        uint8_t fgPixel = 0, fgPalette = 0, fgPriority = 0;

        // Background
        if (_mask.renderBackground && bgLeftAllowed)
        {
            uint16_t bit = 0x8000 >> _fineX;
            bgPixel =
                ((_bgShifterPatternHi & bit) ? 2 : 0) |
                ((_bgShifterPatternLo & bit) ? 1 : 0);

            bgPalette =
                ((_bgShifterAttribHi & bit) ? 2 : 0) |
                ((_bgShifterAttribLo & bit) ? 1 : 0);
        }

        // Sprites
        if (_mask.renderSprites && sprLeftAllowed)
        {
            _spriteZeroBeingRendered = false;
            for (uint8_t i = 0; i < _spriteCount && i < 8; i++)
            {
                if (_spriteScanline[i].x == 0)
                {
                    fgPixel =
                        ((_spriteShifterHi[i] & 0x80) ? 2 : 0) |
                        ((_spriteShifterLo[i] & 0x80) ? 1 : 0);
                    if (fgPixel)
                    {
                        fgPalette = (_spriteScanline[i].attribute & 0x03) + 4;
                        fgPriority = !(_spriteScanline[i].attribute & 0x20);
                        if (i == 0) _spriteZeroBeingRendered = true;
                        break;
                    }
                }
            }
        }

        // Final pixel
        uint8_t pixel = 0, palette = 0;
        if (bgPixel == 0 && fgPixel == 0) { pixel = 0; palette = 0; }
        else if (bgPixel == 0) { pixel = fgPixel; palette = fgPalette; }
        else if (fgPixel == 0) { pixel = bgPixel; palette = bgPalette; }
        else
        {
            if (fgPriority) { pixel = fgPixel; palette = fgPalette; }
            else { pixel = bgPixel; palette = bgPalette; }

            if (_spriteZeroHitPossible && _spriteZeroBeingRendered &&
                _cycle >= 1)
            {
                _status.spriteZeroHit = true;
            }
        }

        if (_pixelCallback)
        {
            Pixel c = GetColorFromPalette(palette, pixel);
            _pixelCallback(_cycle - 1, _scanline, c.r, c.g, c.b);
        }
    }
    

    // --------------------------------------------------------
    // Advance cycle
    // --------------------------------------------------------
    _cycle++;
    if (_cycle >= 341)
    {
        _cycle = 0;
        _scanline++;
        if (_scanline >= 261)
        {
            _scanline = -1;
            _frameComplete = true;
            _oddFrame = !_oddFrame;
        }
    }

    // Optional: scanline hooks
    if (renderingEnabled && _cycle == 260 && _scanline < 240)
        _cart->GetMapper().ScanlineCounter();
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

void PPU2C02::SetCartridge(CartridgeInterfaceAPI* cart) {
    if (cart) {
        Log("Info: Valid CartridgeInterfaceAPI.");
        _cart = cart;
    }
    else {
        Log("Error: Invalid CartridgeInterfaceAPI.");
    }
}


