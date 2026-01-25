#pragma once
#include <cstdint>
#include <cstring>
#include "PPU2C02.Types.h"

// Handles sprite evaluation and rendering
class PPU2C02_Sprites {
protected:
    // OAM (Object Attribute Memory)
    OAMEntry OAM[64];
    uint8_t _oamAddress = 0;

    // Sprite scanline data
    OAMEntry _spriteScanline[8];
    uint8_t _spriteCount = 0;
    uint8_t _spriteShifterLo[8];
    uint8_t _spriteShifterHi[8];

    // Sprite 0 hit detection
    bool _spriteZeroHitPossible = false;
    bool _spriteZeroBeingRendered = false;

public:
    PPU2C02_Sprites();
    virtual ~PPU2C02_Sprites() = default;

    void ResetSprites(bool coldstart);

    // Sprite evaluation and loading
    void EvaluateSprites(int16_t scanline, const PpuControlRegister& control, PpuStatusRegister& status);
    void LoadSpriteShifters(int16_t scanline, const PpuControlRegister& control);
    void UpdateSpriteShifters(const PpuMaskRegister& mask, int16_t cycle);

    // Get sprite pixel
    void GetSpritePixel(uint8_t& pixel, uint8_t& palette, uint8_t& priority, bool& spriteZero);

    // OAM access
    uint8_t ReadOAM() const { return reinterpret_cast<const uint8_t*>(OAM)[_oamAddress]; }
    void WriteOAM(uint8_t data);
    void SetOAMAddress(uint8_t addr) { _oamAddress = addr; }
    uint8_t GetOAMAddress() const { return _oamAddress; }

    // Debug access
    const OAMEntry* GetOAM() const { return OAM; }
    OAMEntry* GetOAMMutable() { return OAM; }  // For DLL exports

    // Sprite 0 hit
    bool IsSpriteZeroHitPossible() const { return _spriteZeroHitPossible; }
    bool IsSpriteZeroBeingRendered() const { return _spriteZeroBeingRendered; }
    void SetSpriteZeroBeingRendered(bool value) { _spriteZeroBeingRendered = value; }

protected:
    // Must be implemented by derived class
    virtual uint8_t PpuRead(uint16_t addr, bool rdOnly = false) = 0;

    // Helper
    static constexpr uint8_t FlipByte(uint8_t b) noexcept;
};