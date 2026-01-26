#pragma once
#include <cstdint>
#include "PPU2C02.Types.h"

// Forward declarations
class CartridgeInterfaceAPI;
struct Pixel;

// Handles all PPU memory operations (nametables, palette RAM, pattern tables)
class PPU2C02_Memory {
protected:
    static constexpr Pixel _systemPalette[64] = {
        Pixel(84, 84, 84), Pixel(0, 30, 116), Pixel(8, 16, 144), Pixel(48, 0, 136),
        Pixel(68, 0, 100), Pixel(92, 0, 48), Pixel(84, 4, 0), Pixel(60, 24, 0),
        Pixel(32, 42, 0), Pixel(8, 58, 0), Pixel(0, 64, 0), Pixel(0, 60, 0),
        Pixel(0, 50, 60), Pixel(0, 0, 0), Pixel(0, 0, 0), Pixel(0, 0, 0),
        Pixel(152, 150, 152), Pixel(8, 76, 196), Pixel(48, 50, 236), Pixel(92, 30, 228),
        Pixel(136, 20, 176), Pixel(160, 20, 100), Pixel(152, 34, 32), Pixel(120, 60, 0),
        Pixel(84, 90, 0), Pixel(40, 114, 0), Pixel(8, 124, 0), Pixel(0, 118, 40),
        Pixel(0, 102, 120), Pixel(0, 0, 0), Pixel(0, 0, 0), Pixel(0, 0, 0),
        Pixel(236, 238, 236), Pixel(76, 154, 236), Pixel(120, 124, 236), Pixel(176, 98, 236),
        Pixel(228, 84, 236), Pixel(236, 88, 180), Pixel(236, 106, 100), Pixel(212, 136, 32),
        Pixel(160, 170, 0), Pixel(116, 196, 0), Pixel(76, 208, 32), Pixel(56, 204, 108),
        Pixel(56, 180, 204), Pixel(60, 60, 60), Pixel(0, 0, 0), Pixel(0, 0, 0),
        Pixel(236, 238, 236), Pixel(168, 204, 236), Pixel(188, 188, 236), Pixel(212, 178, 236),
        Pixel(236, 174, 236), Pixel(236, 174, 212), Pixel(236, 180, 176), Pixel(228, 196, 144),
        Pixel(204, 210, 120), Pixel(180, 222, 120), Pixel(168, 226, 144), Pixel(152, 226, 180),
        Pixel(160, 214, 228), Pixel(160, 162, 160), Pixel(0, 0, 0), Pixel(0, 0, 0)
    };
protected:
    // Memory
    uint8_t _nametable0[1024];
    uint8_t _nametable1[1024];
    uint8_t _nametable2[1024];
    uint8_t _nametable3[1024];
    uint8_t _paletteRam[32];

    // Memory access helpers
    uint8_t _dataBuffer = 0;
    CartridgeInterfaceAPI* _cart = nullptr;

public:
    PPU2C02_Memory();
    virtual ~PPU2C02_Memory() = default;

    void ResetMemory(bool coldstart);

    // PPU bus interface
    uint8_t PpuRead(uint16_t addr, bool rdOnly = false);
    void PpuWrite(uint16_t addr, uint8_t data);

    // Nametable access
    uint8_t ReadNametable(uint16_t addr);
    void WriteNametable(uint16_t addr, uint8_t data);

    // Palette access
    static constexpr Pixel LookupSystemPalette(size_t index);
    Pixel GetColorFromPalette(uint8_t palette, uint8_t pixel);

    // Cartridge
    void SetCartridge(CartridgeInterfaceAPI* cart) { _cart = cart; }

    // Debug access
    uint8_t* GetPaletteRam() { return _paletteRam; }
    const uint8_t* GetPaletteRam() const { return _paletteRam; }
    static constexpr const Pixel* GetSystemPalette() { return _systemPalette; }
};