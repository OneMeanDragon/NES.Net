#pragma once

#include <cstdint>
#include <cstring>

// Diagnostics
#include "Diagnostics/DiagnosticHelpers.h"

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif

// Forward declaration
class CartridgeInterfaceAPI;

constexpr uint16_t VRAM_COARSEX    = 0x001F; //0b0000 0000 0001 1111 5-4-3-2-1    << 0
constexpr uint16_t VRAM_COARSEY    = 0x03E0; //0b0000 0011 1110 0000 10-9-8-7-6   << 5
constexpr uint16_t VRAM_NAMETABLEX = 0x0400; //0b0000 0100 0000 0000 11           << 10
constexpr uint16_t VRAM_NAMETABLEY = 0x0800; //0b0000 1000 0000 0000 12           << 11
constexpr uint16_t VRAM_FINEY      = 0x7000; //0b0111 0000 0000 0000 15-14-13     << 12
constexpr uint16_t VRAM_UNUSED     = 0x8000; //0b1000 0000 0000 0000 16           << 15

constexpr uint16_t PPUCTRL   = 0x0000;
constexpr uint16_t PPUMASK   = 0x0001;
constexpr uint16_t PPUSTATUS = 0x0002;
constexpr uint16_t OAMADDR   = 0x0003;
constexpr uint16_t OAMDATA   = 0x0004;
constexpr uint16_t PPUSCROLL = 0x0005;
constexpr uint16_t PPUADDR   = 0x0006;
constexpr uint16_t PPUDATA   = 0x0007;

constexpr uint8_t CTRL_ENABLENMI         = 0x80; //0b10000000
constexpr uint8_t CTRL_SLAVEMODE         = 0x40; //0b01000000
constexpr uint8_t CTRL_SPRITESIZE        = 0x20; //0b00100000
constexpr uint8_t CTRL_BACKGROUNDPATTERN = 0x10; //0b00010000
constexpr uint8_t CTRL_SPRITEPATTERN     = 0x08; //0b00001000
constexpr uint8_t CTRL_ADDRINC           = 0x04; //0b00000100
constexpr uint8_t CTRL_NAMETABLEY        = 0x02; //0b00000010
constexpr uint8_t CTRL_NAMETABLEX        = 0x01; //0b00000001


// Callback types
typedef void (*PixelCallback)(int x, int y, uint8_t r, uint8_t g, uint8_t b);

#pragma pack(push, 1)
struct Pixel {
    uint8_t r, g, b;

    Pixel() : r(0), g(0), b(0) {}
    Pixel(uint8_t red, uint8_t green, uint8_t blue) : r(red), g(green), b(blue) {}
};

struct OAMEntry {
    uint8_t y;
    uint8_t tileID;
    uint8_t attributes;
    uint8_t x;

    void Fill(uint8_t value) {
        y = value;
        tileID = value;
        attributes = value;
        x = value;
    }

    void CopyFrom(const OAMEntry& other) {
        y = other.y;
        tileID = other.tileID;
        attributes = other.attributes;
        x = other.x;
    }

    uint8_t GetByteAt(uint8_t oamAddr) const {
        switch (oamAddr & 0x03) {
        case 0: return y;
        case 1: return tileID;
        case 2: return attributes;
        case 3: return x;
        default: return 0xFF;
        }
    }

    void SetByteAt(uint8_t oamAddr, uint8_t data) {
        switch (oamAddr & 0x03) {
        case 0: y = data; break;
        case 1: tileID = data; break;
        case 2: attributes = data; break;
        case 3: x = data; break;
        }
    }

    bool IsFlippedVertically() const { return (attributes & 0x80) != 0; }
    bool IsFlippedHorizontally() const { return (attributes & 0x40) != 0; }
};

union PpuControlRegister {
    struct {
        uint8_t nametableX : 1;
        uint8_t nametableY : 1;
        uint8_t incrementMode : 1;
        uint8_t patternSprite : 1;
        uint8_t patternBackground : 1;
        uint8_t spriteSize : 1;
        uint8_t slaveMode : 1;
        uint8_t enableNmi : 1;
    };
    uint8_t reg;

    PpuControlRegister() : reg(0) {}
};

union PpuMaskRegister {
    struct {
        uint8_t grayscale : 1;
        uint8_t renderBackgroundLeft : 1;
        uint8_t renderSpritesLeft : 1;
        uint8_t renderBackground : 1;
        uint8_t renderSprites : 1;
        uint8_t enhanceRed : 1;
        uint8_t enhanceGreen : 1;
        uint8_t enhanceBlue : 1;
    };
    uint8_t reg;

    PpuMaskRegister() : reg(0) {}
};

union PpuStatusRegister {
    struct {
        uint8_t unused : 5;
        uint8_t spriteOverflow : 1;
        uint8_t spriteZeroHit : 1;
        uint8_t verticalBlank : 1;
    };
    uint8_t reg;

    PpuStatusRegister() : reg(0) {}
};

#pragma pack(pop)

class PPU2C02 {
public:
    PPU2C02();
    ~PPU2C02();

    void SetCartridge(CartridgeInterfaceAPI* cart);

    // Core functions
    void Reset(bool coldstart);
    void Clock();

    // CPU interface (mapped to $2000-$2007)
    uint8_t CpuRead(uint16_t addr, bool rdOnly = false);
    void CpuWrite(uint16_t addr, uint8_t data);

    // PPU bus interface ($0000-$3FFF in PPU memory space)
    uint8_t PpuRead(uint16_t addr, bool rdOnly = false);
    void PpuWrite(uint16_t addr, uint8_t data);

    // Frame status
    bool IsFrameComplete() const { return _frameComplete; }
    void SetFrameComplete(bool value) { _frameComplete = value; }

    // NMI/Scanline triggers
    bool GetNmiRequested() const { return _nmiRequested; }
    void ClearNmiRequested() { _nmiRequested = false; }
    bool GetScanlineTrigger() const { return _scanlineTrigger; }
    void ClearScanlineTrigger() { _scanlineTrigger = false; }

    // Callbacks
    void SetPixelCallback(PixelCallback callback) { _pixelCallback = callback; }
    void SetDiagnosticCallback(DiagnosticLogCallback callback) { _diagnosticCallback = callback; }

    // Debug helpers
    void GetPatternTable(uint8_t table, uint8_t palette, uint8_t* buffer);
    void GetNameTable(uint8_t index, uint8_t* buffer);

    // Public OAM access (for external tools and DMA)
    OAMEntry OAM[64];

    // Color palette helper (public for debug tools)
    Pixel GetColorFromPalette(uint8_t palette, uint8_t pixel);

private:
    // Internal PPU RAM
    // Note: Pattern tables are NOT in PPU - they're in cartridge CHR ROM/RAM
    uint8_t _nametable0[1024];  // First internal nametable
    uint8_t _nametable1[1024];  // Second internal nametable
    uint8_t _paletteRam[32];    // Palette RAM (32 bytes, with mirroring)
    Pixel _systemPalette[64];   // NES color palette lookup table

    // PPU Registers
    PpuControlRegister _control;    // $2000 - PPUCTRL
    PpuMaskRegister _mask;          // $2001 - PPUMASK
    PpuStatusRegister _status;      // $2002 - PPUSTATUS

    //uint8_t  _registers[8];
    uint16_t _vramAddr;             // Current VRAM address (v register)
    uint16_t _tramAddr;             // Temporary VRAM address (t register)

    uint8_t _fineX;                 // Fine X scroll (3 bits)
    uint8_t _addressLatch;          // First/second write toggle (w register)
    uint8_t _dataBuffer;            // Read buffer for PPUDATA

    // Timing
    int16_t _scanline;              // Current scanline (-1 to 260)
    int16_t _cycle;                 // Current cycle (0 to 340)
    bool _oddFrame;                 // Odd/even frame flag
    bool _frameComplete;            // Frame rendering complete flag
    bool _nmiRequested;             // NMI interrupt requested
    bool _scanlineTrigger;          // Scanline IRQ for mappers

    // Background rendering pipeline
    uint8_t _bgNextTileId;          // Next tile ID to be loaded
    uint8_t _bgNextTileAttrib;      // Next tile attribute (palette)
    uint8_t _bgNextTileLsb;         // Next tile pattern LSB
    uint8_t _bgNextTileMsb;         // Next tile pattern MSB
    uint16_t _bgShifterPatternLo;   // Background pattern shift register (low)
    uint16_t _bgShifterPatternHi;   // Background pattern shift register (high)
    uint16_t _bgShifterAttribLo;    // Background attribute shift register (low)
    uint16_t _bgShifterAttribHi;    // Background attribute shift register (high)

    // Sprite rendering
    uint8_t _oamAddress;            // OAM address for reads/writes ($2003)
    OAMEntry _spriteScanline[8];    // Sprites on current scanline
    uint8_t _spriteCount;           // Number of sprites found
    uint8_t _spriteShifterLo[8];    // Sprite pattern shift registers (low)
    uint8_t _spriteShifterHi[8];    // Sprite pattern shift registers (high)
    bool _spriteZeroHitPossible;    // Sprite 0 is on this scanline
    bool _spriteZeroBeingRendered;  // Sprite 0 pixel is being drawn now

    // Cartridge reference (not owned)
    CartridgeInterfaceAPI* _cart;

    // Callbacks
    PixelCallback _pixelCallback;
    DiagnosticLogCallback _diagnosticCallback;

    // Helper functions
    void InitializeSystemPalette();

    // Background helpers
    void IncrementScrollX();
    void IncrementScrollY();
    void TransferAddressX();
    void TransferAddressY();
    void LoadBackgroundShifters();
    void UpdateShifters();

    // Sprite helpers
    void EvaluateSprites();
    void LoadSpriteShifters();
    uint8_t FlipByte(uint8_t b);

    // Logging
    void Log(const char* msg);

    // Bus connection (non-owning)
    class NESBus* _bus = nullptr;
public:
    void ConnectBus(class NESBus* bus) { _bus = bus; }
};

// ============================================
// EXPORTED C API FUNCTIONS
// ============================================

DLLEXPORT PPU2C02* CreatePPU();
DLLEXPORT void DestroyPPU(PPU2C02* ppu);
DLLEXPORT void PPU_Reset(PPU2C02* ppu, bool coldstart);
DLLEXPORT void PPU_Clock(PPU2C02* ppu);
DLLEXPORT uint8_t PPU_CpuRead(PPU2C02* ppu, uint16_t addr, bool rdOnly);
DLLEXPORT void PPU_CpuWrite(PPU2C02* ppu, uint16_t addr, uint8_t data);
DLLEXPORT bool PPU_IsFrameComplete(PPU2C02* ppu);
DLLEXPORT void PPU_SetFrameComplete(PPU2C02* ppu, bool value);
DLLEXPORT bool PPU_GetNmiRequested(PPU2C02* ppu);
DLLEXPORT void PPU_ClearNmiRequested(PPU2C02* ppu);
DLLEXPORT void PPU_SetPixelCallback(PPU2C02* ppu, PixelCallback callback);
DLLEXPORT void PPU_SetDiagnosticCallback(PPU2C02* ppu, DiagnosticLogCallback callback);
DLLEXPORT void PPU_GetPatternTable(PPU2C02* ppu, uint8_t table, uint8_t palette, uint8_t* buffer);
DLLEXPORT void PPU_GetOAMEntry(PPU2C02* ppu, uint8_t index, OAMEntry* entry);
DLLEXPORT void PPU_SetOAMEntry(PPU2C02* ppu, uint8_t index, OAMEntry* entry);
DLLEXPORT uint8_t PPU_GetOAMByte(PPU2C02* ppu, uint8_t oamAddr);
DLLEXPORT void PPU_SetOAMByte(PPU2C02* ppu, uint8_t oamAddr, uint8_t data);
DLLEXPORT void PPU_GetColorFromPalette(PPU2C02* ppu, uint8_t palette, uint8_t pixel, uint8_t* r, uint8_t* g, uint8_t* b);