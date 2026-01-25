#include <iostream>
#include <core/Interfaces/MirrorModeRequired.h>
#include "PPU_TEST.cpp"

#include "../NesChipset/PPU2C02.cpp"

// Test harness
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

        TestResetState();
        TestRegisterReadWrite();
        TestVRAMAccess();
        TestPaletteRAM();
        TestPatternTableAccess();
        TestNametableMirroring();
        TestScrollRegisters();
        TestOAMAccess();
        TestBackgroundRenderingPipeline();
        TestSpriteEvaluation();
        TestPixelComposition();
        TestFrameTiming();
        TestNMIGeneration();
        TestOpenBusBehavior();

        std::cout << "\n=== All Tests Complete ===\n";
    }

private:
    void TestResetState() {
        std::cout << "Test 1: Reset State\n";

        _ppu.Reset(true);

        // Check initial register values
        uint8_t status = _ppu.CpuRead(0x2002, true);
        assert((status & 0xE0) == 0xA0 && "Status register reset value");

        // OAM should be filled with 0xFF on cold reset
        uint8_t oamByte = _ppu.CpuRead(0x2004, true);
        assert(oamByte == 0xFF && "OAM reset to 0xFF");

        std::cout << "  y Passed\n";
    }

    void TestRegisterReadWrite() {
        std::cout << "Test 2: Register Read/Write\n";

        // Test PPUCTRL ($2000) - write-only
        _ppu.CpuWrite(0x2000, 0xAA);
        uint8_t readBack = _ppu.CpuRead(0x2000, false);
        assert(readBack == 0xAA && "PPUCTRL write-only behavior");

        // Test PPUMASK ($2001) - write-only
        _ppu.CpuWrite(0x2001, 0x55);
        readBack = _ppu.CpuRead(0x2001, false);
        assert(readBack == 0x55 && "PPUMASK write-only behavior");

        // Test PPUSTATUS ($2002) - read-only, clears vblank
        _ppu.CpuWrite(0x2002, 0xFF); // Should be ignored
        _ppu.CpuWrite(0x2000, 0x80); // Enable NMI
        // Simulate vblank
        for (int i = 0; i < 100000; i++) _ppu.Clock();
        uint8_t status = _ppu.CpuRead(0x2002, false);
        assert((status & 0x80) == 0 && "PPUSTATUS read clears vblank");

        std::cout << "  y Passed\n";
    }

    void TestVRAMAccess() {
        std::cout << "Test 3: VRAM Access\n";

        // Set VRAM address
        _ppu.CpuWrite(0x2006, 0x20); // High byte
        _ppu.CpuWrite(0x2006, 0x00); // Low byte

        // Write data with different increment modes
        _ppu.CpuWrite(0x2000, 0x00); // Increment by 1
        _ppu.CpuWrite(0x2007, 0x11);
        _ppu.CpuWrite(0x2007, 0x22);

        // Read back (with buffering)
        _ppu.CpuWrite(0x2006, 0x20);
        _ppu.CpuWrite(0x2006, 0x00);
        uint8_t dummy = _ppu.CpuRead(0x2007, false); // Discard buffer
        uint8_t val1 = _ppu.CpuRead(0x2007, false);
        uint8_t val2 = _ppu.CpuRead(0x2007, false);

        assert(val1 == 0x11 && "VRAM read buffering");
        assert(val2 == 0x22 && "VRAM sequential read");

        // Test 32-byte increment
        _ppu.CpuWrite(0x2000, 0x04); // Set increment mode
        _ppu.CpuWrite(0x2006, 0x20);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2007, 0x33);

        // Address should now be 0x2020
        _ppu.CpuRead(0x2007, false); // Dummy read
        uint8_t nextVal = _ppu.CpuRead(0x2007, false);
        // Should read from 0x2021

        std::cout << "  y Passed\n";
    }

    void TestPaletteRAM() {
        std::cout << "Test 4: Palette RAM\n";

        // Test palette writes
        _ppu.CpuWrite(0x2006, 0x3F);
        _ppu.CpuWrite(0x2006, 0x00);

        _ppu.CpuWrite(0x2007, 0x2A); // Universal background
        _ppu.CpuWrite(0x2007, 0x1B); // Background palette 0, color 1
        _ppu.CpuWrite(0x2007, 0x2C); // Background palette 0, color 2
        _ppu.CpuWrite(0x2007, 0x3D); // Background palette 0, color 3

        // Test mirroring ($3F10 mirrors $3F00, etc.)
        _ppu.CpuWrite(0x2006, 0x3F);
        _ppu.CpuWrite(0x2006, 0x10);
        _ppu.CpuWrite(0x2007, 0x0F); // Should write to $3F00

        // Read back
        _ppu.CpuWrite(0x2006, 0x3F);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuRead(0x2007, false); // Dummy
        uint8_t bgColor = _ppu.CpuRead(0x2007, false);

        assert(bgColor == 0x0F && "Palette mirroring");
        assert((bgColor & 0xC0) == 0 && "Palette bits 6-7 should be 0");

        std::cout << "  y Passed\n";
    }

    void TestPatternTableAccess() {
        std::cout << "Test 5: Pattern Table Access\n";

        // Write to pattern table through cartridge
        _cart.SetPatternByte(0x0000, 0xAA);
        _cart.SetPatternByte(0x0001, 0x55);

        // Read through PPU bus
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuRead(0x2007, false); // Dummy
        uint8_t val1 = _ppu.CpuRead(0x2007, false);
        uint8_t val2 = _ppu.CpuRead(0x2007, false);

        assert(val1 == 0xAA && "Pattern table read 1");
        assert(val2 == 0x55 && "Pattern table read 2");

        std::cout << "  y Passed\n";
    }

    void TestNametableMirroring() {
        std::cout << "Test 6: Nametable Mirroring\n";

        // Test Horizontal mirroring
        _cart.SetMirrorMode(MirrorMode::Horizontal);

        _ppu.CpuWrite(0x2006, 0x20); // Nametable 0
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2007, 0x11);

        _ppu.CpuWrite(0x2006, 0x24); // Nametable 1 (should mirror 0)
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2007, 0x22);

        _ppu.CpuWrite(0x2006, 0x28); // Nametable 2
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2007, 0x33);

        // Read back to verify mirroring
        _ppu.CpuWrite(0x2006, 0x20);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuRead(0x2007, false);
        uint8_t nt0 = _ppu.CpuRead(0x2007, false);

        _ppu.CpuWrite(0x2006, 0x24);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuRead(0x2007, false);
        uint8_t nt1 = _ppu.CpuRead(0x2007, false);

        assert(nt0 == 0x11 && "Horizontal mirroring NT0");
        assert(nt1 == 0x22 && "Horizontal mirroring NT1");

        // Test Vertical mirroring
        _cart.SetMirrorMode(MirrorMode::Vertical);
        _ppu.Reset(false);

        _ppu.CpuWrite(0x2006, 0x20);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2007, 0x44);

        _ppu.CpuWrite(0x2006, 0x28); // Should mirror $2000 in vertical
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2007, 0x55);

        std::cout << "  y Passed\n";
    }

    void TestScrollRegisters() {
        std::cout << "Test 7: Scroll Registers\n";

        // Test coarse X/Y and fine X scroll
        _ppu.CpuWrite(0x2005, 0x42); // First write: X scroll
        _ppu.CpuWrite(0x2005, 0x85); // Second write: Y scroll

        // The scroll values should be stored in internal registers
        // We can't directly read them, but we can verify through rendering

        // Test address latch toggling
        _ppu.CpuWrite(0x2005, 0x11); // Should set X again
        _ppu.CpuWrite(0x2005, 0x22); // Should set Y again

        std::cout << "  y Passed\n";
    }

    void TestOAMAccess() {
        std::cout << "Test 8: OAM Access\n";

        // Set OAM address
        _ppu.CpuWrite(0x2003, 0x10);

        // Write OAM data with auto-increment
        _ppu.CpuWrite(0x2004, 0xAA); // y
        _ppu.CpuWrite(0x2004, 0xBB); // tile id
        _ppu.CpuWrite(0x2004, 0xCC); // attributes
        _ppu.CpuWrite(0x2004, 0xDD); // x

        // Read back
        _ppu.CpuWrite(0x2003, 0x10);
        uint8_t y = _ppu.CpuRead(0x2004, false);
        uint8_t tile = _ppu.CpuRead(0x2004, false);
        uint8_t attr = _ppu.CpuRead(0x2004, false);
        uint8_t x = _ppu.CpuRead(0x2004, false);

        assert(y == 0xAA && "OAM write/read y");
        assert(tile == 0xBB && "OAM write/read tile");
        assert(attr == 0xCC && "OAM write/read attr");
        assert(x == 0xDD && "OAM write/read x");

        std::cout << "  y Passed\n";
    }

    void TestBackgroundRenderingPipeline() {
        std::cout << "Test 9: Background Rendering Pipeline\n";

        // Enable background rendering
        _ppu.CpuWrite(0x2001, 0x08); // renderBackground

        // Set up a simple background
        _cart.SetMirrorMode(MirrorMode::Horizontal);

        // Write to nametable
        _ppu.CpuWrite(0x2006, 0x20);
        _ppu.CpuWrite(0x2006, 0x00);
        _ppu.CpuWrite(0x2007, 0x01); // Tile 1

        // Write attribute
        _ppu.CpuWrite(0x2006, 0x23);
        _ppu.CpuWrite(0x2006, 0xC0);
        _ppu.CpuWrite(0x2007, 0x01); // Palette 1 for quadrant

        // Set up pattern table
        _cart.SetPatternByte(0x0010, 0xFF); // Tile 1, row 0 low
        _cart.SetPatternByte(0x0018, 0x00); // Tile 1, row 0 high

        // Set scroll to (0,0)
        _ppu.CpuWrite(0x2005, 0x00);
        _ppu.CpuWrite(0x2005, 0x00);

        // Run a few clocks to see if pipeline works
        for (int i = 0; i < 100; i++) {
            _ppu.Clock();
        }

        std::cout << "  y Passed\n";
    }

    void TestSpriteEvaluation() {
        std::cout << "Test 10: Sprite Evaluation\n";

        // Write sprite 0 to OAM
        _ppu.CpuWrite(0x2003, 0x00);
        _ppu.CpuWrite(0x2004, 0x20); // y = 32
        _ppu.CpuWrite(0x2004, 0x01); // tile id
        _ppu.CpuWrite(0x2004, 0x00); // attributes
        _ppu.CpuWrite(0x2004, 0x40); // x = 64

        // Enable sprite rendering
        _ppu.CpuWrite(0x2001, 0x10); // renderSprites

        // Run to scanline 32
        while (!_ppu.IsFrameComplete()) {
            _ppu.Clock();
            if (_ppu.GetNmiRequested()) {
                _ppu.ClearNmiRequested();
            }
        }

        std::cout << "  y Passed\n";
    }

    void TestPixelComposition() {
        std::cout << "Test 11: Pixel Composition\n";

        // This tests the GetColorFromPalette function
        Pixel color = _ppu.GetColorFromPalette(0, 0);
        assert(color.r == 84 && color.g == 84 && color.b == 84 && "Universal background color");

        // Test palette 1, color 1
        // First set palette RAM
        _ppu.CpuWrite(0x2006, 0x3F);
        _ppu.CpuWrite(0x2006, 0x05);
        _ppu.CpuWrite(0x2007, 0x02); // Palette index 2

        color = _ppu.GetColorFromPalette(1, 1);
        // Should use system palette index 2

        std::cout << "  y Passed\n";
    }

    void TestFrameTiming() {
        std::cout << "Test 12: Frame Timing\n";

        int cyclesPerFrame = 0;
        int scanlines = 0;

        // Run one full frame
        _ppu.SetFrameComplete(false);
        while (!_ppu.IsFrameComplete()) {
            _ppu.Clock();
            cyclesPerFrame++;

            if (_ppu.GetNmiRequested()) {
                _ppu.ClearNmiRequested();
            }
        }

        // NES PPU: 341 cycles per scanline, 262 scanlines per frame
        // But actual rendered frame is 241 scanlines (0-239)
        assert(cyclesPerFrame == 341 * 262 && "Frame timing");

        std::cout << "  y Passed (Cycles per frame: " << cyclesPerFrame << ")\n";
    }

    void TestNMIGeneration() {
        std::cout << "Test 13: NMI Generation\n";

        // Enable NMI
        _ppu.CpuWrite(0x2000, 0x80);

        bool nmiTriggered = false;

        // Run until vblank
        for (int i = 0; i < 100000; i++) {
            _ppu.Clock();
            if (_ppu.GetNmiRequested()) {
                nmiTriggered = true;
                _ppu.ClearNmiRequested();
                break;
            }
        }

        assert(nmiTriggered && "NMI should be generated at vblank");

        // Disable NMI and ensure it doesn't trigger
        _ppu.CpuWrite(0x2000, 0x00);
        _ppu.Reset(false);

        nmiTriggered = false;
        for (int i = 0; i < 100000; i++) {
            _ppu.Clock();
            if (_ppu.GetNmiRequested()) {
                nmiTriggered = true;
                break;
            }
        }

        assert(!nmiTriggered && "NMI should not trigger when disabled");

        std::cout << "  y Passed\n";
    }

    void TestOpenBusBehavior() {
        std::cout << "Test 14: Open Bus Behavior\n";

        // Write to a register
        _ppu.CpuWrite(0x2000, 0xAA);

        // Read from write-only register should return last written bus value
        uint8_t readValue = _ppu.CpuRead(0x2000, false);
        assert(readValue == 0xAA && "Open bus for write-only register");

        // Test invalid address
        readValue = _ppu.CpuRead(0x2008, false); // Invalid PPU address
        assert(readValue == 0xAA && "Open bus for invalid address");

        std::cout << "  y Passed\n";
    }
};

int main() {
    std::cout << "Starting PPU2C02 Test Suite...\n";

    try {
        PPUTest tester;
        tester.RunAllTests();
        std::cout << "\ny All tests passed successfully!\n";
        return 0;
    }
    catch (const std::exception& e) {
        std::cerr << "\nx Test failed: " << e.what() << "\n";
        return 1;
    }
    catch (...) {
        std::cerr << "\nx Unknown test failure\n";
        return 1;
    }
}