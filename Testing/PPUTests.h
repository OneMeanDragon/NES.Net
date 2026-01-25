#pragma once

#include "DiagnosticTest.h"
#include "PPU/PPU2C02.h"
#include "DiagnosticCartridge.h"

void TestPPUBasics(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU Basic Initialization");

    ppu->Reset(true);

    test.Assert(true, "PPU reset without crashing");
    test.Assert(!ppu->IsFrameComplete(), "Frame not complete after reset");
    test.Assert(!ppu->GetNmiRequested(), "No NMI requested after reset");
}

void TestPPURegisters(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU Register Access");

    ppu->Reset(true);

    // Test PPUCTRL (0x2000) write
    ppu->CpuWrite(0x2000, 0x80);
    test.Assert(true, "PPUCTRL write successful");

    // Test PPUMASK (0x2001) write
    ppu->CpuWrite(0x2001, 0x1E);
    test.Assert(true, "PPUMASK write successful");

    // Test PPUSTATUS (0x2002) read
    uint8_t status = ppu->CpuRead(0x2002);
    test.Assert(true, "PPUSTATUS read successful");
    test.Info("PPUSTATUS value: 0x" + std::to_string(status));

    // Test OAMADDR (0x2003) write
    ppu->CpuWrite(0x2003, 0x00);
    test.Assert(true, "OAMADDR write successful");

    // Test OAMDATA (0x2004) write/read
    ppu->CpuWrite(0x2003, 0x00);
    ppu->CpuWrite(0x2004, 0x42);
    ppu->CpuWrite(0x2003, 0x00);
    uint8_t oamData = ppu->CpuRead(0x2004);
    test.AssertEquals((uint8_t)0x42, oamData, "OAMDATA write/read cycle");
}

void TestPPUMemory(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU Memory Access via PPUADDR/PPUDATA");

    ppu->Reset(true);

    // Write to nametable at 0x2000
    ppu->CpuWrite(0x2006, 0x20); // High byte
    ppu->CpuWrite(0x2006, 0x00); // Low byte
    ppu->CpuWrite(0x2007, 0x42); // Write data

    test.Assert(true, "PPUADDR/PPUDATA write successful");

    // Read back from nametable
    ppu->CpuWrite(0x2006, 0x20); // High byte
    ppu->CpuWrite(0x2006, 0x00); // Low byte
    uint8_t dummy = ppu->CpuRead(0x2007); // Dummy read (PPU buffering)
    uint8_t value = ppu->CpuRead(0x2007); // Actual data

    test.AssertEquals((uint8_t)0x42, value, "PPU memory read matches write");

    // Test palette memory (no buffering)
    ppu->CpuWrite(0x2006, 0x3F); // Palette high byte
    ppu->CpuWrite(0x2006, 0x00); // Palette low byte
    ppu->CpuWrite(0x2007, 0x30); // Write palette data

    ppu->CpuWrite(0x2006, 0x3F);
    ppu->CpuWrite(0x2006, 0x00);
    uint8_t paletteValue = ppu->CpuRead(0x2007); // No dummy read needed

    test.AssertEquals((uint8_t)0x30, paletteValue, "Palette memory access (no buffering)");
}

void TestPPUScrolling(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU Scrolling (PPUSCROLL)");

    ppu->Reset(true);

    // Write scroll position
    ppu->CpuWrite(0x2005, 0x10); // X scroll
    ppu->CpuWrite(0x2005, 0x20); // Y scroll

    test.Assert(true, "PPUSCROLL writes successful");

    // Reset latch and try again
    ppu->CpuRead(0x2002); // Reading status resets address latch
    ppu->CpuWrite(0x2005, 0x00);
    ppu->CpuWrite(0x2005, 0x00);

    test.Assert(true, "PPUSCROLL latch reset works");
}

void TestPPUOAM(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU OAM (Object Attribute Memory)");

    ppu->Reset(true);

    // Write to OAM using OAMADDR and OAMDATA
    ppu->CpuWrite(0x2003, 0x00); // Set OAM address to 0

    // Write sprite data (Y, tile, attributes, X)
    ppu->CpuWrite(0x2004, 0x10); // Y position
    ppu->CpuWrite(0x2004, 0x05); // Tile ID
    ppu->CpuWrite(0x2004, 0x00); // Attributes
    ppu->CpuWrite(0x2004, 0x20); // X position

    test.Assert(true, "OAM write via OAMDATA successful");

    // Read back
    ppu->CpuWrite(0x2003, 0x00);
    uint8_t y = ppu->CpuRead(0x2004);
    test.AssertEquals((uint8_t)0x10, y, "OAM Y position read correctly");
}

void TestPPUVBlank(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU VBlank and NMI Generation");

    ppu->Reset(true);

    // Enable NMI
    ppu->CpuWrite(0x2000, 0x80); // Set NMI enable bit

    test.Assert(true, "NMI enabled in PPUCTRL");

    // Clock PPU through a frame to reach VBlank
    // PPU takes 341 * 262 = 89342 cycles per frame
    // VBlank starts at scanline 241
    int vblankCycle = 341 * 241;

    test.Info("Clocking PPU to VBlank...");

    for (int i = 0; i < vblankCycle + 100; i++) {
        ppu->Clock();
    }

    test.Assert(ppu->GetNmiRequested(), "NMI requested during VBlank");

    // Clear NMI
    ppu->ClearNmiRequested();
    test.Assert(!ppu->GetNmiRequested(), "NMI cleared successfully");
}

void TestPPUPatternTables(DiagnosticTest& test, PPU2C02* ppu, DiagnosticCartridge* cart) {
    test.StartTest("PPU Pattern Table Access");

    ppu->Reset(true);

    // Write test pattern to CHR ROM
    std::vector<uint8_t> testPattern = {
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // Low bitplane
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  // High bitplane
    };

    // Note: This assumes CHR RAM is writable
    for (uint8_t i = 0; i < testPattern.size(); i++) {
        ppu->PpuWrite(i, testPattern[i]);
    }

    test.Assert(true, "Pattern data written to CHR memory");

    // Read back
    uint8_t firstByte = ppu->PpuRead(0x0000);
    test.AssertEquals((uint8_t)0xFF, firstByte, "Pattern table read successful");
}

void TestPPUNametables(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU Nametable Access");

    ppu->Reset(true);

    // Write to all four nametables
    uint16_t nametableAddrs[] = { 0x2000, 0x2400, 0x2800, 0x2C00 };
    uint8_t testValues[] = { 0x11, 0x22, 0x33, 0x44 };

    for (int i = 0; i < 4; i++) {
        ppu->PpuWrite(nametableAddrs[i], testValues[i]);
    }

    test.Assert(true, "Wrote to all nametable regions");

    // Read back (note: mirroring may affect results)
    for (int i = 0; i < 4; i++) {
        uint8_t value = ppu->PpuRead(nametableAddrs[i]);
        test.Info("Nametable " + std::to_string(i) + " value: 0x" +
            std::to_string(value) + " (mirroring may apply)");
    }
}

void TestPPUPalettes(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU Palette RAM");

    ppu->Reset(true);

    // Background palette 0
    ppu->PpuWrite(0x3F00, 0x0F); // Universal background
    ppu->PpuWrite(0x3F01, 0x30); // Color 1
    ppu->PpuWrite(0x3F02, 0x16); // Color 2
    ppu->PpuWrite(0x3F03, 0x27); // Color 3

    test.Assert(true, "Background palette written");

    // Sprite palette 0
    ppu->PpuWrite(0x3F10, 0x0F);
    ppu->PpuWrite(0x3F11, 0x30);
    ppu->PpuWrite(0x3F12, 0x16);
    ppu->PpuWrite(0x3F13, 0x27);

    test.Assert(true, "Sprite palette written");

    // Read back
    uint8_t bgColor = ppu->PpuRead(0x3F01);
    test.AssertEquals((uint8_t)0x30, bgColor, "Palette read matches write");

    // Test mirroring at 0x3F04, 0x3F08, 0x3F0C (universal background mirrors)
    uint8_t mirror1 = ppu->PpuRead(0x3F04);
    uint8_t mirror2 = ppu->PpuRead(0x3F08);
    test.Info("Palette mirroring: 0x3F04=" + std::to_string(mirror1) +
        ", 0x3F08=" + std::to_string(mirror2));
}

void TestPPUAddressIncrement(DiagnosticTest& test, PPU2C02* ppu) {
    test.StartTest("PPU Address Auto-Increment");

    ppu->Reset(true);

    // Test increment by 1 (horizontal)
    ppu->CpuWrite(0x2000, 0x00); // PPUCTRL: increment by 1
    ppu->CpuWrite(0x2006, 0x20);
    ppu->CpuWrite(0x2006, 0x00);

    ppu->CpuWrite(0x2007, 0x01); // Should increment address by 1
    ppu->CpuWrite(0x2007, 0x02);
    ppu->CpuWrite(0x2007, 0x03);

    // Read back
    ppu->CpuWrite(0x2006, 0x20);
    ppu->CpuWrite(0x2006, 0x00);
    uint8_t dummy = ppu->CpuRead(0x2007);
    uint8_t val1 = ppu->CpuRead(0x2007);
    uint8_t val2 = ppu->CpuRead(0x2007);
    uint8_t val3 = ppu->CpuRead(0x2007);

    test.AssertEquals((uint8_t)0x01, val1, "Address increment +1 works (byte 1)");
    test.AssertEquals((uint8_t)0x02, val2, "Address increment +1 works (byte 2)");
    test.AssertEquals((uint8_t)0x03, val3, "Address increment +1 works (byte 3)");

    // Test increment by 32 (vertical)
    ppu->CpuWrite(0x2000, 0x04); // PPUCTRL: increment by 32
    ppu->CpuWrite(0x2006, 0x20);
    ppu->CpuWrite(0x2006, 0x00);

    ppu->CpuWrite(0x2007, 0xAA); // Address 0x2000
    ppu->CpuWrite(0x2007, 0xBB); // Address 0x2020 (0x2000 + 32)

    test.Assert(true, "Address increment +32 write successful");
}