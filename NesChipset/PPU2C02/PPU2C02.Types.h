#pragma once
#include <cstdint>

// ============================================================================
// Common types and structures used across PPU components
// ============================================================================

#pragma pack(push, 1)

// RGB Pixel structure
struct Pixel {
    uint8_t r, g, b;

    // constexpr default constructor
    constexpr Pixel() : r(0), g(0), b(0) {}

    // constexpr value constructor
    constexpr Pixel(uint8_t red, uint8_t green, uint8_t blue) : r(red), g(green), b(blue) {}
};

// OAM Entry structure (sprite attributes)
struct OAMEntryS {
    uint8_t y;
    uint8_t id;
    uint8_t attribute;
    uint8_t x;
};
union OAMEntry {
    OAMEntryS entries[64];      // Structured access
    uint8_t raw[64 * 4];       // Raw byte access
};

// PPU Control Register ($2000)
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

// PPU Mask Register ($2001)
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

// PPU Status Register ($2002)
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

// Loopy Register (internal VRAM address)
//union LoopyRegister {
//    struct {
//        uint16_t coarseX : 5;
//        uint16_t coarseY : 5;
//        uint16_t nametableX : 1;
//        uint16_t nametableY : 1;
//        uint16_t fineY : 3;
//        uint16_t unused : 1;
//    };
//    uint16_t reg;
//
//    LoopyRegister() : reg(0) {}
//};
struct LoopyRegister { /* should never go over 0x7fff */
    uint16_t reg;

    LoopyRegister() : reg(0) {}

    // Getters
    uint16_t GetCoarseX() const { return reg & 0x1Fu; }
    uint16_t GetCoarseY() const { return (reg >> 5) & 0x1Fu; }
    uint16_t GetFineY() const { return (reg >> 12) & 0x7u; }

    uint16_t GetNametableX() const { return (reg >> 10) & 0x1u; }
    uint16_t GetNametableY() const { return (reg >> 11) & 0x1u; }

    // Setters
    void SetCoarseX(uint16_t val) {
        reg = (reg & ~0x1Fu) | (val & 0x1Fu);
    }
    void SetCoarseY(uint16_t val) {
        reg = (reg & ~0x3e0u) | ((val & 0x1Fu) << 5);
    }
    void SetNametableX(uint16_t val) {
        reg = (reg & ~0x400u) | ((val & 0x1u) << 10);
    }
    void SetNametableY(uint16_t val) {
        reg = (reg & ~0x800u) | ((val & 0x1u) << 11);
    }
    void SetFineY(uint16_t val) {
        reg = (reg & ~0x7000u) | ((val & 0x7u) << 12);
    }

    // Convenience
    void IncrementCoarseY() {
        uint16_t y = (reg >> 5) & 0x1Fu;
        if (y == 29) {
            reg &= ~0x3e0u;
            reg ^= 0x0800u;
        }
        else if (y == 31) {
            reg &= ~0x3e0u;
        }
        else {
            reg += 0x20u;
        }
    }

    void IncrementCoarseX() {
        if ((reg & 0x001Fu) == 31) {
            reg &= ~0x001Fu;
            reg ^= 0x0400u;
        }
        else {
            reg++;
        }
    }

    void IncrementFineY() {
        if ((reg & 0x7000u) != 0x7000u) {
            reg += 0x1000u;
        }
        else {
            reg &= ~0x7000u;
            IncrementCoarseY();
        }
    }

};
#pragma pack(pop)
