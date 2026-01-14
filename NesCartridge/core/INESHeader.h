#pragma once

#include <cstdint>

#pragma region Constants
constexpr size_t KB_SIZE = 1024;
constexpr size_t INES_HEADER_SIZE = 16;
constexpr size_t PRG_BANK_SIZE = KB_SIZE * 16;  // 16KB
constexpr size_t CHR_BANK_SIZE = KB_SIZE * 8;   // 8KB
constexpr size_t TRAINER_SIZE = 512;
#pragma endregion


#pragma pack(push, 1)
struct INESHeader {
    uint8_t magic[4];          // "NES" + $1A
    uint8_t prg_rom_size;      // PRG ROM size in 16KB units
    uint8_t chr_rom_size;      // CHR ROM size in 8KB units
    uint8_t flags6;            // Mapper, mirroring, battery, trainer
    uint8_t flags7;            // Mapper, VS/Playchoice, NES 2.0
    uint8_t flags8;            // PRG-RAM size
    uint8_t flags9;            // TV system
    uint8_t flags10;           // TV system, PRG-RAM presence
    uint8_t unused[5];         // Unused padding

    // --- Property Helpers ---

    uint8_t get_mapper_number() const {
        // High nibble of flags7 (upper 4 bits) and High nibble of flags6 (lower 4 bits)
        return (flags7 & 0xF0) | (flags6 >> 4);
    }

    bool is_vertical_mirroring() const {
        return (flags6 & 0x01) != 0;
    }

    bool has_battery_backed_ram() const {
        return (flags6 & 0x02) != 0;
    }

    bool has_trainer() const {
        return (flags6 & 0x04) != 0;
    }

    bool is_four_screen_mode() const {
        return (flags6 & 0x08) != 0;
    }

    bool is_nes2_format() const {
        return (flags7 & 0x0C) == 0x08;
    }

    bool is_valid() const {
        return magic[0] == 0x4E && // 'N'
            magic[1] == 0x45 &&    // 'E'
            magic[2] == 0x53 &&    // 'S'
            magic[3] == 0x1A;      // EOF
    }
};
#pragma pack(pop)