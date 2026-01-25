#pragma once
#include <cstdint>

#pragma region Constants
constexpr size_t KB_SIZE          = 1024;
constexpr size_t INES_HEADER_SIZE = 16;
constexpr size_t EIGHT_KB         = KB_SIZE * 8;
constexpr size_t SIXTEEN_KB       = KB_SIZE * 16;
constexpr size_t PRG_BANK_SIZE    = SIXTEEN_KB;     // 16KB
constexpr size_t CHR_BANK_SIZE    = EIGHT_KB;       // 8KB
constexpr size_t TRAINER_SIZE     = 512;
#pragma endregion

#pragma pack(push, 1)
struct INESHeader {
    uint8_t magic[4]{ 0 };          // "NES" + $1A
    uint8_t prg_rom_size{ 0 };      // PRG ROM size in 16KB units (iNES 1.0) or LSB (NES 2.0)
    uint8_t chr_rom_size{ 0 };      // CHR ROM size in 8KB units (iNES 1.0) or LSB (NES 2.0)
    uint8_t flags6{ 0 };            // Mapper, mirroring, battery, trainer
    uint8_t flags7{ 0 };            // Mapper, VS/Playchoice, NES 2.0
    uint8_t flags8{ 0 };            // iNES 1.0: PRG-RAM size | NES 2.0: Mapper MSB + Submapper
    uint8_t flags9{ 0 };            // iNES 1.0: TV system   | NES 2.0: PRG/CHR ROM MSB
    uint8_t flags10{ 0 };           // iNES 1.0: Unofficial  | NES 2.0: PRG/CHR RAM sizes
    uint8_t flags11{ 0 };           // NES 2.0: CHR-RAM (volatile) shift count
    uint8_t flags12{ 0 };           // NES 2.0: CHR-NVRAM (battery) shift count
    uint8_t flags13{ 0 };           // NES 2.0: Timing mode
    uint8_t flags14{ 0 };           // NES 2.0: VS/Extended console type
    uint8_t flags15{ 0 };           // NES 2.0: Misc ROMs
    uint8_t flags16{ 0 };           // NES 2.0: Default expansion device

    // --- Validation ---
    bool is_valid() const {
        return magic[0] == 0x4E && // 'N'
            magic[1] == 0x45 &&    // 'E'
            magic[2] == 0x53 &&    // 'S'
            magic[3] == 0x1A;      // EOF
    }

    bool is_nes2_format() const {
        return (flags7 & 0x0C) == 0x08;
    }

    // --- Mapper ---
    uint16_t get_mapper_number() const {
        if (is_nes2_format()) {
            // NES 2.0: 12-bit mapper (bits from flags6, flags7, flags8)
            return ((flags8 & 0x0F) << 8) | (flags7 & 0xF0) | (flags6 >> 4);
        }
        else {
            // iNES 1.0: 8-bit mapper
            return (flags7 & 0xF0) | (flags6 >> 4);
        }
    }

    uint8_t get_submapper() const {
        if (is_nes2_format()) {
            return (flags8 >> 4) & 0x0F;
        }
        return 0;
    }

    // --- PRG ROM Size ---
    size_t get_prg_rom_size() const {
        if (is_nes2_format()) {
            // NES 2.0: PRG-ROM size with MSB in flags9
            uint32_t lsb = prg_rom_size;
            uint32_t msb = (flags9 & 0x0F);

            if (msb == 0x0F) {
                // Exponent-multiplier notation
                uint32_t multiplier = (lsb & 0x03) * 2 + 1;
                uint32_t exponent = (lsb >> 2);
                return static_cast<size_t>(multiplier) << exponent;
            }
            else {
                // Standard notation
                return static_cast<size_t>((msb << 8) | lsb) * PRG_BANK_SIZE;
            }
        }
        else {
            // iNES 1.0
            if (prg_rom_size == 0) return 0;
            return static_cast<size_t>(prg_rom_size) * PRG_BANK_SIZE;
        }
    }

    // --- CHR ROM Size ---
    size_t get_chr_rom_size() const {
        if (is_nes2_format()) {
            // NES 2.0: CHR-ROM size with MSB in flags9
            uint32_t lsb = chr_rom_size;
            uint32_t msb = (flags9 >> 4) & 0x0F;

            if (msb == 0x0F) {
                // Exponent-multiplier notation
                uint32_t multiplier = (lsb & 0x03) * 2 + 1;
                uint32_t exponent = (lsb >> 2);
                return static_cast<size_t>(multiplier) << exponent;
            }
            else {
                // Standard notation
                return static_cast<size_t>((msb << 8) | lsb) * CHR_BANK_SIZE;
            }
        }
        else {
            // iNES 1.0
            if (chr_rom_size == 0) return 0;
            return static_cast<size_t>(chr_rom_size) * CHR_BANK_SIZE;
        }
    }

    // --- PRG-RAM / PRG-NVRAM Size ---
    size_t get_prg_ram_size() const {
        if (is_nes2_format()) {
            uint8_t shift = flags10 & 0x0F;
            if (shift == 0) return 0;
            return 64ULL << shift;  // 2^(shift + 6) bytes
        }
        else {
            // iNES 1.0: flags8 * 8KB (default to 8KB if 0)
            uint8_t size_8k = flags8;
            if (size_8k == 0) size_8k = 1;
            return static_cast<size_t>(size_8k) * EIGHT_KB;
        }
    }

    size_t get_prg_nvram_size() const {
        if (is_nes2_format()) {
            uint8_t shift = (flags10 >> 4) & 0x0F;
            if (shift == 0) return 0;
            return 64ULL << shift;
        }
        return 0;  // iNES 1.0 doesn't specify this separately
    }

    // --- CHR-RAM / CHR-NVRAM Size ---
    size_t get_chr_ram_size() const {
        if (is_nes2_format()) {
            uint8_t shift = flags11 & 0x0F;
            if (shift == 0 && chr_rom_size == 0) {
                // Default to 8KB if no CHR-ROM
                return EIGHT_KB;
            }
            if (shift == 0) return 0;
            return 64ULL << shift;
        }
        else {
            // iNES 1.0: if chr_rom_size == 0, assume 8KB CHR-RAM
            if (chr_rom_size == 0) {
                return EIGHT_KB;
            }
            return 0;
        }
    }

    size_t get_chr_nvram_size() const {
        if (is_nes2_format()) {
            uint8_t shift = (flags11 >> 4) & 0x0F;
            if (shift == 0) return 0;
            return 64ULL << shift;
        }
        return 0;
    }

    // --- Mirroring ---
    bool is_vertical_mirroring() const {
        return (flags6 & 0x01) != 0;
    }

    bool is_four_screen_mode() const {
        return (flags6 & 0x08) != 0;
    }

    // --- Other Flags ---
    bool has_battery_backed_ram() const {
        return (flags6 & 0x02) != 0;
    }

    bool has_trainer() const {
        return (flags6 & 0x04) != 0;
    }

    // --- TV System ---
    enum class TVSystem : uint8_t {
        NTSC = 0,
        PAL = 1,
        Dual = 2,
        Dendy = 3
    };

    TVSystem get_tv_system() const {
        if (is_nes2_format()) {
            return static_cast<TVSystem>(flags12 & 0x03);
        }
        else {
            // iNES 1.0: bit 0 of flags9
            return (flags9 & 0x01) ? TVSystem::PAL : TVSystem::NTSC;
        }
    }
};
#pragma pack(pop)