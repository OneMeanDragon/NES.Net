#pragma once
#include <cstdint>
#include "PPU2C02.Types.h"

// Handles background rendering pipeline
class PPU2C02_Background {
protected:
    // Background tile data
    uint8_t _bgNextTileId = 0;
    uint8_t _bgNextTileAttrib = 0;
    uint8_t _bgNextTileLsb = 0;
    uint8_t _bgNextTileMsb = 0;

    // Background shifters
    uint16_t _bgShifterPatternLo = 0;
    uint16_t _bgShifterPatternHi = 0;
    uint16_t _bgShifterAttribLo = 0;
    uint16_t _bgShifterAttribHi = 0;

protected:
    // Fetch operations - must be implemented by derived class
    virtual uint8_t PpuRead(uint16_t addr, bool rdOnly = false) = 0;

public:
    PPU2C02_Background() = default;
    virtual ~PPU2C02_Background() = default;

    void ResetBackground();

    // Background rendering helpers
    void IncrementScrollX(LoopyRegister& vramAddr);
    void IncrementScrollY(LoopyRegister& vramAddr);
    void TransferAddressX(LoopyRegister& vramAddr, const LoopyRegister& tramAddr);
    void TransferAddressY(LoopyRegister& vramAddr, const LoopyRegister& tramAddr);
    void LoadBackgroundShifters();
    void UpdateBackgroundShifters(const PpuMaskRegister& mask, int16_t cycle);

    // Fetch pipeline
    void FetchNametableByte(const LoopyRegister& vramAddr);
    void FetchAttributeByte(const LoopyRegister& vramAddr);

    uint16_t GetPatternAddress(const LoopyRegister& vramAddr, const PpuControlRegister& control, bool highPlane);
    void FetchPatternLow(const LoopyRegister& vramAddr, const PpuControlRegister& control);
    void FetchPatternHigh(const LoopyRegister& vramAddr, const PpuControlRegister& control);

    // Get pixel data
    void GetBackgroundPixel(uint8_t fineX, uint8_t& pixel, uint8_t& palette) const;
};