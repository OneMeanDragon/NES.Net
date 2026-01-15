#pragma once

#include <cstdint>
#include <cstring>

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif

// Forward declaration
class Cartridge;
class MapperBase;
class CartridgeInterface;

// Callback types
typedef void (*PixelCallback)(int x, int y, uint8_t r, uint8_t g, uint8_t b);
typedef void (*DiagnosticCallback)(const char* msg);

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

union LoopyRegister {
    struct {
        uint16_t coarseX : 5;
        uint16_t coarseY : 5;
        uint16_t nametableX : 1;
        uint16_t nametableY : 1;
        uint16_t fineY : 3;
        uint16_t unused : 1;
    };
    uint16_t reg;

    LoopyRegister() : reg(0) {}
};
#pragma pack(pop)

class PPU2C02 {
public:
    PPU2C02(Cartridge* cart);
    ~PPU2C02();

    // Core functions
    void Reset();
    void Clock();

    // CPU interface
    uint8_t CpuRead(uint16_t addr, bool rdOnly = false);
    void CpuWrite(uint16_t addr, uint8_t data);

    // PPU bus interface
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
    void SetDiagnosticCallback(DiagnosticCallback callback) { _diagnosticCallback = callback; }

    // Debug helpers
    void GetPatternTable(uint8_t table, uint8_t palette, uint8_t* buffer);
    void GetNameTable(uint8_t index, uint8_t* buffer);

    // Public OAM access (for external tools)
    OAMEntry OAM[64];

private:
    // Memory
    uint8_t _nametable0[1024];
    uint8_t _nametable1[1024];
    uint8_t _paletteRam[32];
    uint8_t _patternTable0[4096];
    uint8_t _patternTable1[4096];
    Pixel _systemPalette[64];

    // Registers & State
    PpuControlRegister _control;
    PpuMaskRegister _mask;
    PpuStatusRegister _status;
    LoopyRegister _vramAddr;
    LoopyRegister _tramAddr;
    uint8_t _fineX;
    uint8_t _addressLatch;
    uint8_t _dataBuffer;
    int16_t _scanline;
    int16_t _cycle;
    bool _oddFrame;
    bool _frameComplete;
    bool _nmiRequested;
    bool _scanlineTrigger;

    // Background rendering
    uint8_t _bgNextTileId;
    uint8_t _bgNextTileAttrib;
    uint8_t _bgNextTileLsb;
    uint8_t _bgNextTileMsb;
    uint16_t _bgShifterPatternLo;
    uint16_t _bgShifterPatternHi;
    uint16_t _bgShifterAttribLo;
    uint16_t _bgShifterAttribHi;

    // Sprite rendering
    uint8_t _oamAddress;
    OAMEntry _spriteScanline[8];
    uint8_t _spriteCount;
    uint8_t _spriteShifterLo[8];
    uint8_t _spriteShifterHi[8];
    bool _spriteZeroHitPossible;
    bool _spriteZeroBeingRendered;

    // Cartridge reference
    CartridgeInterface* _cart;

    // Callbacks
    PixelCallback _pixelCallback;
    DiagnosticCallback _diagnosticCallback;

    // Helper functions
    void InitializeSystemPalette();

public: /* for API call */
    Pixel GetColorFromPalette(uint8_t palette, uint8_t pixel);
private:
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
};
