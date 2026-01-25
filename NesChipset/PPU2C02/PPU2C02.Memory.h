#pragma once
#include <cstdint>
#include "PPU2C02.Types.h"

// Forward declarations
class CartridgeInterfaceAPI;
struct Pixel;

// Handles all PPU memory operations (nametables, palette RAM, pattern tables)
class PPU2C02_Memory {
protected:
    // Memory
    uint8_t _nametable0[1024];
    uint8_t _nametable1[1024];
    uint8_t _nametable2[1024];
    uint8_t _nametable3[1024];
    uint8_t _paletteRam[32];
    Pixel _systemPalette[64];

    // Memory access helpers
    uint8_t _dataBuffer = 0;
    CartridgeInterfaceAPI* _cart = nullptr;

public:
    PPU2C02_Memory();
    virtual ~PPU2C02_Memory() = default;

    void InitializeSystemPalette();
    void ResetMemory(bool coldstart);

    // PPU bus interface
    uint8_t PpuRead(uint16_t addr, bool rdOnly = false);
    void PpuWrite(uint16_t addr, uint8_t data);

    // Nametable access
    uint8_t ReadNametable(uint16_t addr);
    void WriteNametable(uint16_t addr, uint8_t data);

    // Palette access
    Pixel GetColorFromPalette(uint8_t palette, uint8_t pixel);

    // Cartridge
    void SetCartridge(CartridgeInterfaceAPI* cart) { _cart = cart; }

    // Debug access
    uint8_t* GetPaletteRam() { return _paletteRam; }
    const uint8_t* GetPaletteRam() const { return _paletteRam; }
    Pixel* GetSystemPalette() { return _systemPalette; }
    const Pixel* GetSystemPalette() const { return _systemPalette; }
};