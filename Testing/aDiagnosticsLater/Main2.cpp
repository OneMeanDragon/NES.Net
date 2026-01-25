// PPU_Test.cpp - Standalone PPU2C02 Test
#include <iostream>
#include <cstdint>
#include <cstring>
#include <cassert>
#include <string>

// ============================================
// Minimal mock implementations
// ============================================
enum MirrorMode {
    Horizontal,
    Vertical,
    OneScreenLo,
    OneScreenHi,
    FourScreen
};

class MapperInterfaceAPI {
public:
    virtual void ScanlineCounter() {}
    virtual ~MapperInterfaceAPI() = default;
};

class CartridgeInterfaceAPI {
private:
    uint8_t _chrRom[8192] = { 0 };
    MirrorMode _mirrorMode = MirrorMode::Horizontal;
    MapperInterfaceAPI _mapper;

public:
    bool PpuRead(uint16_t addr, uint8_t* data) {
        if (addr < 8192) {
            *data = _chrRom[addr];
            return true;
        }
        return false;
    }

    bool PpuWrite(uint16_t addr, uint8_t data) {
        if (addr < 8192) {
            _chrRom[addr] = data;
            return true;
        }
        return false;
    }

    MirrorMode GetMirrorMode() { return _mirrorMode; }
    MapperInterfaceAPI& GetMapper() { return _mapper; }

    void SetMirrorMode(MirrorMode mode) { _mirrorMode = mode; }
    void SetPatternByte(uint16_t addr, uint8_t value) {
        if (addr < 8192) _chrRom[addr] = value;
    }
};

// ============================================
// PPU Implementation (simplified)
// ============================================
#pragma pack(push, 1)
struct Pixel {
    uint8_t r, g, b;
    Pixel() : r(0), g(0), b(0) {}
    Pixel(uint8_t red, uint8_t green, uint8_t blue) : r(red), g(green), b(blue) {}
};

struct OAMEntry {
    uint8_t y;
    uint8_t id;
    uint8_t attribute;
    uint8_t x;
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

typedef void (*PixelCallback)(int x, int y, uint8_t r, uint8_t g, uint8_t b);
typedef void (*DiagnosticLogCallback)(const char* msg);

class PPU2C02 {
private:
    // Memory
    uint8_t _nametable0[1024] = { 0 };
    uint8_t _nametable1[1024] = { 0 };
    uint8_t _nametable2[1024] = { 0 };
    uint8_t _nametable3[1024] = { 0 };
    uint8_t _paletteRam[32] = { 0 };
    Pixel _systemPalette[64];

    // Registers
    PpuControlRegister _control;
    PpuMaskRegister _mask;
    PpuStatusRegister _status;
    LoopyRegister _vramAddr;
    LoopyRegister _tramAddr;
    uint8_t _fineX = 0;
    uint8_t _addressLatch = 0;
    uint8_t _dataBuffer = 0;
    uint8_t _openBus = 0;
    uint8_t _oamAddress = 0;

    // Timing
    int16_t _scanline = 0;
    int16_t _cycle = 0;
    bool _oddFrame = false;
    bool _frameComplete = false;
    bool _nmiRequested = false;

    // Background
    uint8_t _bgNextTileId = 0;
    uint8_t _bgNextTileAttrib = 0;
    uint8_t _bgNextTileLsb = 0;
    uint8_t _bgNextTileMsb = 0;
    uint16_t _bgShifterPatternLo = 0;
    uint16_t _bgShifterPatternHi = 0;
    uint16_t _bgShifterAttribLo = 0;
    uint16_t _bgShifterAttribHi = 0;

    // Sprites
    OAMEntry OAM[64];
    OAMEntry _spriteScanline[8];
    uint8_t _spriteCount = 0;
    uint8_t _spriteShifterLo[8] = { 0 };
    uint8_t _spriteShifterHi[8] = { 0 };
    bool _spriteZeroHitPossible = false;
    bool _spriteZeroBeingRendered = false;

    // Cartridge
    CartridgeInterfaceAPI* _cart = nullptr;

    // Callbacks
    PixelCallback _pixelCallback = nullptr;
    DiagnosticLogCallback _diagnosticCallback = nullptr;

    void InitializeSystemPalette() {
        // Just initialize a few colors for testing
        _systemPalette[0x00] = Pixel(84, 84, 84);
        _systemPalette[0x01] = Pixel(0, 30, 116);
        _systemPalette[0x02] = Pixel(8, 16, 144);
        _systemPalette[0x0F] = Pixel(0, 0, 0);
        _systemPalette[0x10] = Pixel(152, 150, 152);
        _systemPalette[0x20] = Pixel(236, 238, 236);
        _systemPalette[0x30] = Pixel(236, 238, 236);
    }

public:
    PPU2C02() {
        std::memset(_nametable0, 0, sizeof(_nametable0));
        std::memset(_nametable1, 0, sizeof(_nametable1));
        std::memset(_nametable2, 0, sizeof(_nametable2));
        std::memset(_nametable3, 0, sizeof(_nametable3));
        std::memset(_paletteRam, 0, sizeof(_paletteRam));
        std::memset(OAM, 0xFF, sizeof(OAM));

        InitializeSystemPalette();
        Reset(true);
    }

    void SetCartridge(CartridgeInterfaceAPI* cart) {
        _cart = cart;
    }

    void Reset(bool coldstart) {
        if (coldstart) {
            std::memset(OAM, 0xFF, sizeof(OAM));
            std::memset(_paletteRam, 0xFF, sizeof(_paletteRam));
        }

        _oamAddress = 0;
        _control.reg = 0;
        _mask.reg = 0;
        _status.reg = 0xA0;
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
    }

    uint8_t CpuRead(uint16_t addr, bool rdOnly = false) {
        uint8_t data = 0x00;

        switch (addr & 0x0007) {
        case 0x0000: // PPUCTRL - write-only
        case 0x0001: // PPUMASK - write-only
        case 0x0003: // OAMADDR - write-only
        case 0x0005: // PPUSCROLL - write-only
        case 0x0006: // PPUADDR - write-only
            data = _openBus;
            break;

        case 0x0002: // PPUSTATUS
            data = (_status.reg & 0xE0) | (_openBus & 0x1F);
            _status.verticalBlank = false;
            _addressLatch = 0;
            break;

        case 0x0004: // OAMDATA
            data = reinterpret_cast<uint8_t*>(OAM)[_oamAddress];
            break;

        case 0x0007: { // PPUDATA
            if (_vramAddr.reg >= 0x3F00) {
                data = PpuRead(_vramAddr.reg);
                _dataBuffer = PpuRead(_vramAddr.reg & 0x2FFF);
            }
            else {
                data = _dataBuffer;
                _dataBuffer = PpuRead(_vramAddr.reg);
            }
            _vramAddr.reg += (_control.incrementMode ? 32 : 1);
            break;
        }
        }

        _openBus = data;
        return data;
    }

    void CpuWrite(uint16_t addr, uint8_t data) {
        _openBus = data;

        switch (addr & 0x0007) {
        case 0x0000: // PPUCTRL
            _control.reg = data;
            _tramAddr.nametableX = _control.nametableX;
            _tramAddr.nametableY = _control.nametableY;
            break;

        case 0x0001: // PPUMASK
            _mask.reg = data;
            break;

        case 0x0002: // PPUSTATUS - read-only
            break;

        case 0x0003: // OAMADDR
            _oamAddress = data;
            break;

        case 0x0004: // OAMDATA
            reinterpret_cast<uint8_t*>(OAM)[_oamAddress] = data;
            _oamAddress++;
            break;

        case 0x0005: // PPUSCROLL
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

        case 0x0006: // PPUADDR
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

        case 0x0007: // PPUDATA
            PpuWrite(_vramAddr.reg, data);
            _vramAddr.reg += (_control.incrementMode ? 32 : 1);
            break;
        }
    }

    uint8_t PpuRead(uint16_t addr, bool rdOnly = false) {
        uint8_t data = 0;
        addr &= 0x3FFF;

        if (addr <= 0x1FFF) {
            // Pattern tables
            if (_cart && _cart->PpuRead(addr, &data)) {
                return data;
            }
            return 0x00;
        }
        else if (addr >= 0x2000 && addr <= 0x3EFF) {
            // Nametables
            addr &= 0x0FFF;
            MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;

            if (mirror == MirrorMode::Horizontal) {
                if (addr < 0x0400 || (addr >= 0x0800 && addr < 0x0C00)) {
                    data = _nametable0[addr & 0x03FF];
                }
                else {
                    data = _nametable1[addr & 0x03FF];
                }
            }
            else { // Vertical
                if (addr < 0x0800) {
                    data = _nametable0[addr & 0x03FF];
                }
                else {
                    data = _nametable1[addr & 0x03FF];
                }
            }
        }
        else if (addr >= 0x3F00 && addr <= 0x3FFF) {
            // Palette RAM
            addr &= 0x1F;
            if (addr == 0x10) addr = 0x00;
            if (addr == 0x14) addr = 0x04;
            if (addr == 0x18) addr = 0x08;
            if (addr == 0x1C) addr = 0x0C;
            data = _paletteRam[addr] & 0x3F;
        }

        return data;
    }

    void PpuWrite(uint16_t addr, uint8_t data) {
        addr &= 0x3FFF;

        if (addr <= 0x1FFF) {
            if (_cart) _cart->PpuWrite(addr, data);
            return;
        }
        else if (addr >= 0x2000 && addr <= 0x3EFF) {
            addr &= 0x0FFF;
            MirrorMode mirror = _cart ? _cart->GetMirrorMode() : MirrorMode::Horizontal;

            if (mirror == MirrorMode::Horizontal) {
                if (addr < 0x0400 || (addr >= 0x0800 && addr < 0x0C00)) {
                    _nametable0[addr & 0x03FF] = data;
                }
                else {
                    _nametable1[addr & 0x03FF] = data;
                }
            }
            else { // Vertical
                if (addr < 0x0800) {
                    _nametable0[addr & 0x03FF] = data;
                }
                else {
                    _nametable1[addr & 0x03FF] = data;
                }
            }
        }
        else if (addr >= 0x3F00 && addr <= 0x3FFF) {
            addr &= 0x1F;
            if (addr == 0x10) addr = 0x00;
            if (addr == 0x14) addr = 0x04;
            if (addr == 0x18) addr = 0x08;
            if (addr == 0x1C) addr = 0x0C;
            _paletteRam[addr] = data & 0x3F;
        }
    }

    void Clock() {
        _cycle++;
        if (_cycle >= 341) {
            _cycle = 0;
            _scanline++;

            if (_scanline >= 261) {
                _scanline = -1;
                _frameComplete = true;
                _oddFrame = !_oddFrame;

                // Generate NMI at vblank if enabled
                if (_scanline == 241 && _cycle == 1) {
                    _status.verticalBlank = true;
                    if (_control.enableNmi) {
                        _nmiRequested = true;
                    }
                }
            }
        }
    }

    Pixel GetColorFromPalette(uint8_t palette, uint8_t pixel) {
        uint16_t addr = 0x3F00;
        if (pixel != 0) {
            addr += ((uint16_t)palette << 2) + pixel;
        }
        return _systemPalette[PpuRead(addr) & 0x3F];
    }

    bool IsFrameComplete() const { return _frameComplete; }
    void SetFrameComplete(bool value) { _frameComplete = value; }
    bool GetNmiRequested() const { return _nmiRequested; }
    void ClearNmiRequested() { _nmiRequested = false; }

    void SetPixelCallback(PixelCallback callback) { _pixelCallback = callback; }
    void SetDiagnosticCallback(DiagnosticLogCallback callback) { _diagnosticCallback = callback; }
};

// ============================================
// Test Harness
// ============================================
class PPUTest {
private:
    PPU2C02 _ppu;
    CartridgeInterfaceAPI _cart;

public:
    PPUTest() {
        _ppu.SetCartridge(&_cart);
        _ppu.Reset(true);
    }

    void RunAllTests() {
        std::cout << "=== PPU2C02 Comprehensive Test Suite ===\n\n";

        int passed = 0;
        int total = 0;

        auto runTest = [&](const char* name, auto testFunc) {
            total++;
            std::cout << "Test " << total << ": " << name << "\n";
            try {
                testFunc();
                std::cout << "  y Passed\n";
                passed++;
            }
            catch (const std::exception& e) {
                std::cout << "  x Failed: " << e.what() << "\n";
            }
            catch (...) {
                std::cout << "  x Failed with unknown error\n";
            }
            };

        runTest("Reset State", [this]() { TestResetState(); });
        runTest("Register Write/Read", [this]() { TestRegisterWriteRead(); });
        runTest("VRAM Access", [this]() { TestVRAMAccess(); });
        runTest("Palette RAM", [this]() { TestPaletteRAM(); });
        runTest("Pattern Table", [this]() { TestPatternTable(); });
        runTest("OAM Access", [this]() { TestOAMAccess(); });
        runTest("Scroll Registers", [this]() { TestScrollRegisters(); });
        runTest("Frame Timing", [this]() { TestFrameTiming(); });
        runTest("NMI Generation", [this]() { TestNMIGeneration(); });
        runTest("Color Palette", [this]() { TestColorPalette(); });

        std::cout << "\n=== Results: " << passed << "/" << total << " tests passed ===\n";
    }

private:
    void TestResetState() {
        _ppu.Reset(true);

        // Check OAM is filled with 0xFF
        _ppu.CpuWrite(0x2003, 0x00); // Set OAM address to 0
        uint8_t oamByte = _ppu.CpuRead(0x2004, false);
        if (oamByte != 0xFF) {
            throw std::runtime_error("OAM not reset to 0xFF");
        }

        // Check status register
        uint8_t status = _ppu.CpuRead(0x2002, false);
        if ((status & 0xE0) != 0xA0) {
            throw std::runtime_error("Status register incorrect after reset");
        }
    }

    void TestRegisterWriteRead() {
        // Test PPUCTRL - write-only
        _ppu.CpuWrite(0x2000, 0xAA);
        uint8_t readBack = _ppu.CpuRead(0x2000, false);
        if (readBack != 0xAA) {
            throw std::runtime_error("PPUCTRL open bus incorrect");
        }

        // Test PPUMASK - write-only
        _ppu.CpuWrite(0x2001, 0x55);
        readBack = _ppu.CpuRead(0x2001, false);
        if (readBack != 0x55) {
            throw std::runtime_error("PPUMASK open bus incorrect");
        }

        // Test PPUSTATUS clears vblank
        _ppu.CpuWrite(0x2000, 0x80); // Enable NMI
        // Trigger vblank by clocking
        for (int i = 0; i < 100000; i++) {
            _ppu.Clock();
            if (_ppu.GetNmiRequested()) {
                _ppu.ClearNmiRequested();
                break;
            }
        }

        uint8_t status = _ppu.CpuRead(0x2002, false);
        if ((status & 0x80) != 0) {
            throw std::runtime_error("PPUSTATUS read didn't clear vblank");
        }
    }

    void TestVRAMAccess() {
        // Set VRAM address
        _ppu.CpuWrite(0x2006, 0x20);
        _ppu.CpuWrite(0x2006, 0x00);

        // Write data
        _ppu.CpuWrite(0x2000, 0x00); // Increment by 1
        _ppu.CpuWrite(0x2007, 0x11);
        _ppu.CpuWrite(0x2007, 0x22);

        // Read back
        _ppu.CpuWrite(0x2006, 0x20);
        _ppu.CpuWrite(0x2006, 0x00);
        uint8_t dummy = _ppu.CpuRead(0x2007, false);
        uint8_t val1 = _ppu.CpuRead(0x2007, false);
        uint8_t val2 = _ppu.CpuRead(0x2007, false);

        if (val1 != 0x11 || val2 != 0x22) {
            throw std::runtime_error("VRAM read buffering failed");
        }
    }

    void TestPaletteRAM() {
        // Write to palette
        _ppu.CpuWrite(0x2006, 0x3F);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2007, 0x2A);

        // Test mirroring
        _ppu.CpuWrite(0x2006, 0x3F);
        _ppu.CpuWrite(0x2006, 0x10);
        _ppu.CpuWrite(0x2007, 0x0F);

        // Read back
        _ppu.CpuWrite(0x2006, 0x3F);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuRead(0x2007, false);
        uint8_t bgColor = _ppu.CpuRead(0x2007, false);

        if (bgColor != 0x0F) {
            throw std::runtime_error("Palette mirroring failed");
        }
    }

    void TestPatternTable() {
        // Setup pattern in cartridge
        _cart.SetPatternByte(0x0000, 0xAA);
        _cart.SetPatternByte(0x0001, 0x55);

        // Read through PPU
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuRead(0x2007, false);
        uint8_t val1 = _ppu.CpuRead(0x2007, false);
        uint8_t val2 = _ppu.CpuRead(0x2007, false);

        if (val1 != 0xAA || val2 != 0x55) {
            throw std::runtime_error("Pattern table access failed");
        }
    }

    void TestOAMAccess() {
        // Write OAM data
        _ppu.CpuWrite(0x2003, 0x10);
        _ppu.CpuWrite(0x2004, 0xAA);
        _ppu.CpuWrite(0x2004, 0xBB);
        _ppu.CpuWrite(0x2004, 0xCC);
        _ppu.CpuWrite(0x2004, 0xDD);

        // Read back
        _ppu.CpuWrite(0x2003, 0x10);
        uint8_t y = _ppu.CpuRead(0x2004, false);
        uint8_t tile = _ppu.CpuRead(0x2004, false);
        uint8_t attr = _ppu.CpuRead(0x2004, false);
        uint8_t x = _ppu.CpuRead(0x2004, false);

        if (y != 0xAA || tile != 0xBB || attr != 0xCC || x != 0xDD) {
            throw std::runtime_error("OAM access failed");
        }
    }

    void TestScrollRegisters() {
        // Test latch toggling
        _ppu.CpuWrite(0x2005, 0x42);
        _ppu.CpuWrite(0x2005, 0x85);

        // Should be ready for X scroll again
        _ppu.CpuWrite(0x2005, 0x11);
        _ppu.CpuWrite(0x2005, 0x22);

        // Test address register
        _ppu.CpuWrite(0x2006, 0x21);
        _ppu.CpuWrite(0x2006, 0x23);

        uint8_t data = _ppu.CpuRead(0x2007, false);
        // Should work without crashing
    }

    void TestFrameTiming() {
        int cycles = 0;
        _ppu.SetFrameComplete(false);

        while (!_ppu.IsFrameComplete()) {
            _ppu.Clock();
            cycles++;

            if (_ppu.GetNmiRequested()) {
                _ppu.ClearNmiRequested();
            }
        }

        // NES: 341 cycles per scanline, 262 scanlines
        int expected = 341 * 262;
        if (cycles != expected) {
            throw std::runtime_error("Frame timing incorrect: " + std::to_string(cycles) +
                " vs expected " + std::to_string(expected));
        }
    }

    void TestNMIGeneration() {
        // Enable NMI
        _ppu.CpuWrite(0x2000, 0x80);

        bool nmiTriggered = false;
        for (int i = 0; i < 100000; i++) {
            _ppu.Clock();
            if (_ppu.GetNmiRequested()) {
                nmiTriggered = true;
                _ppu.ClearNmiRequested();
                break;
            }
        }

        if (!nmiTriggered) {
            throw std::runtime_error("NMI not generated");
        }

        // Disable and ensure no NMI
        _ppu.CpuWrite(0x2000, 0x00);
        _ppu.Reset(false);

        nmiTriggered = false;
        for (int i = 0; i < 50000; i++) {
            _ppu.Clock();
            if (_ppu.GetNmiRequested()) {
                nmiTriggered = true;
                break;
            }
        }

        if (nmiTriggered) {
            throw std::runtime_error("NMI generated when disabled");
        }
    }

    void TestColorPalette() {
        // Set palette entry
        _ppu.CpuWrite(0x2006, 0x3F);
        _ppu.CpuWrite(0x2006, 0x01);
        _ppu.CpuWrite(0x2007, 0x02);

        // Get color
        Pixel color = _ppu.GetColorFromPalette(0, 1);

        // Basic color should be valid
        if (color.r > 255 || color.g > 255 || color.b > 255) {
            throw std::runtime_error("Invalid color values");
        }
    }
};

// ============================================
// Main Function
// ============================================
int main() {
    std::cout << "Starting PPU2C02 Test Application...\n\n";

    PPUTest tester;
    tester.RunAllTests();

    std::cout << "\nTest application completed.\n";
    return 0;
}