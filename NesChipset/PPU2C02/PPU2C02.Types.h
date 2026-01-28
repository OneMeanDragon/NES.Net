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

namespace PPU {
    enum class PpuState {
        NOTSET,
        VISIBLE,
        PRERENDER,
        VBLANK_NMI,
        POST
    };
}

namespace NTMASKS {
    // Address breakdown: 0x2000-0x2FFF (nametable space)
    // After masking with 0x0FFF: 0x000-0xFFF
    // Bits: xxxx TT oooooooooo
    //       |    |  +--- Offset within nametable (0x000-0x3FF, 10 bits)
    //       |    +------ Table select (0-3, 2 bits, bits 10-11)
    //       +----------- Extended address bits (unused after 0x0FFF mask)

    constexpr uint16_t OFFSET = 0b0000001111111111; // 0x03FF - offset within a nametable
    constexpr uint16_t TABLE = 0b0000110000000000; // 0x0C00 - which of 4 nametables
    constexpr uint16_t INDEX = 0b0000111111111111; // 0x0FFF - full index after mirroring

    // Individual table select bits
    constexpr uint16_t TABLE_BIT0 = 0b0000010000000000; // 0x0400 - horizontal mirror check
    constexpr uint16_t TABLE_BIT1 = 0b0000100000000000; // 0x0800 - vertical mirror check

    // For extracting table number
    constexpr uint8_t  TABLE_SHIFT = 10;                 // Shift right by 10 to get table 0-3
    constexpr uint8_t  TABLE_MASK = 0x03;               // Mask after shift to get 0-3

    // Mirroring helpers
    constexpr uint8_t  HORIZONTAL_MASK = 0x01;             // After (table >> 1), keeps NT0/NT1
    constexpr uint8_t  VERTICAL_MASK = 0x01;             // After (table & 1), keeps NT0/NT1
}

namespace PPUADDR {
    constexpr uint16_t NAMETABLE_BASE      = 0x2000;
    constexpr uint16_t NAMETABLE_MASK      = 0x0FFF;
    constexpr uint16_t ATTR_TABLE_BASE_NT0 = 0x23C0;
}

namespace LOOPYMASKS {
    // Loopy register breakdown: 15-bit PPU address register
    // Format: yyy NN YYYYY XXXXX
    //         |   |  |     +--- Coarse X scroll (tile column 0-31, 5 bits)
    //         |   |  +--------- Coarse Y scroll (tile row 0-29, 5 bits)
    //         |   +------------ Nametable select (2 bits: horizontal=bit 0, vertical=bit 1)
    //         +---------------- Fine Y scroll (pixel row within tile 0-7, 3 bits)
    // 
    // Bits:  15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    //         U  F  F  F  Y  X  Y  Y  Y  Y  Y  X  X  X  X  X
    //         |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
    //         |  +--+--+  |  |  +--+--+--+--+  +--+--+--+--+
    //         |     |     |  |        |              |
    //         |  Fine Y   |  |    Coarse Y       Coarse X
    //         |           |  |
    //      Unused    Table Y  Table X

    constexpr uint16_t COARSEX       = 0b0000000000011111;
    constexpr uint16_t COARSEY       = 0b0000001111100000;
    constexpr uint16_t TABLEX        = 0b0000010000000000;
    constexpr uint16_t TABLEY        = 0b0000100000000000;
    constexpr uint16_t FINEY         = 0b0111000000000000;
    constexpr uint16_t UNUSED        = 0b1000000000000000;
    constexpr uint16_t ALL           = (COARSEX | COARSEY | TABLEX | TABLEY | FINEY | UNUSED);
    constexpr uint16_t TRANSFERX     = (COARSEX | TABLEX);
    constexpr uint16_t TRANSFERY     = (COARSEY | TABLEY | FINEY);

    constexpr uint16_t CLEAR_COARSEX = (ALL & ~COARSEX);
    constexpr uint16_t CLEAR_COARSEY = (ALL & ~COARSEY);
    constexpr uint16_t CLEAR_TABLEX  = (ALL & ~TABLEX);
    constexpr uint16_t CLEAR_TABLEY  = (ALL & ~TABLEY);
    constexpr uint16_t CLEAR_FINEY   = (ALL & ~FINEY);
}

struct LoopyRegister {
    uint16_t reg;

    LoopyRegister() : reg(0) {}

    // Getters
    uint16_t CoarseX() const { return reg & LOOPYMASKS::COARSEX; }
    uint16_t CoarseY() const { return ((reg & LOOPYMASKS::COARSEY) >> 5) & 0x1Fu; }
    uint16_t NametableX() const { return ((reg & LOOPYMASKS::TABLEX) >> 10) & 0x1u; }
    uint16_t NametableY() const { return ((reg & LOOPYMASKS::TABLEY) >> 11) & 0x1u; }
    uint16_t FineY() const { return ((reg & LOOPYMASKS::FINEY) >> 12) & 0x7u; }

    // Setters
    void SetCoarseX(uint16_t val) {
        reg = (reg & LOOPYMASKS::CLEAR_COARSEX) | ((val /*& 31 redundant due to location*/) & LOOPYMASKS::COARSEX);
    }
    void SetCoarseY(uint16_t val) {
        reg = (reg & LOOPYMASKS::CLEAR_COARSEY) | (((val & 31) << 5) & LOOPYMASKS::COARSEY);
    }
    void SetNametableX(uint16_t val) {
        reg = (reg & LOOPYMASKS::CLEAR_TABLEX) | (((val & 1) << 10) & LOOPYMASKS::TABLEX);
    }
    void SetNametableY(uint16_t val) {
        reg = (reg & LOOPYMASKS::CLEAR_TABLEY) | (((val & 1) << 11) & LOOPYMASKS::TABLEY);
    }
    void SetFineY(uint16_t val) {
        reg = (reg & LOOPYMASKS::CLEAR_FINEY) | (((val & 7) << 12) & LOOPYMASKS::FINEY);
    }

    // Convenience
    void IncrementCoarseY() {
        uint16_t coarsey = ((reg & LOOPYMASKS::COARSEY) >> 5) & 0x1Fu;
        if (coarsey == 29) {
            reg &= LOOPYMASKS::CLEAR_COARSEY; // coarsey = 0
            reg ^= LOOPYMASKS::TABLEY; // vert nametable swap
        }
        else if (coarsey == 31) {
            reg &= LOOPYMASKS::CLEAR_COARSEY; // coarsey = 0
        }
        else {
            coarsey++;
            reg = (reg & LOOPYMASKS::CLEAR_COARSEY) | (((coarsey & 0x1Fu) << 5) & LOOPYMASKS::COARSEY);
        }
    }

    void IncrementCoarseX() {
        uint16_t coarsex = reg & LOOPYMASKS::COARSEX;  // extract coarse X
        if (coarsex == 31) {
            reg &= LOOPYMASKS::CLEAR_COARSEX; // coarsex = 0
            reg ^= LOOPYMASKS::TABLEX; // horz nametable swap
        }
        else {
            coarsex++;
            reg = (reg & LOOPYMASKS::CLEAR_COARSEX) | ((coarsex) & LOOPYMASKS::COARSEX);
        }
    }

    void IncrementFineY() {
        uint16_t finey = ((reg & LOOPYMASKS::FINEY) >> 12) & 0x7u;
        if (finey < 7) {
            reg = (reg & LOOPYMASKS::CLEAR_FINEY) | ((++finey << 12) & LOOPYMASKS::FINEY); // pre inc
        }
        else {
            reg &= LOOPYMASKS::CLEAR_FINEY; // finey = 0
            IncrementCoarseY(); // increment coarsey
        }
    }

    // --- Transfers ---
    void CopyHorizontalFrom(const LoopyRegister& src) {
        reg = (reg & ~(LOOPYMASKS::TRANSFERX)) | (src.reg & (LOOPYMASKS::TRANSFERX));
    }
    void CopyVerticalFrom(const LoopyRegister& src) {
        reg = (reg & ~(LOOPYMASKS::TRANSFERY)) | (src.reg & (LOOPYMASKS::TRANSFERY));
    }
};
#pragma pack(pop)
