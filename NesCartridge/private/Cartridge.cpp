#include "../core/Cartridge.h"
#include "../core/mappers/MapperFactory.h"

#include <iostream>
#include <fstream>
#include <sstream>

#include <format>

void Cartridge::ResetState() {
    _isLoaded = false;
    _mapper.reset();

    _romData.clear();
    _prgRom.clear();
    _prgRam.clear();
    _chrRom.clear();
    _chrRam.clear();
    _filepath.clear();

    _trainer = std::span<uint8_t>();

    std::memset(&_header, 0, sizeof(INESHeader));

    Log("Cartridge has been reset.");
}

bool Cartridge::Load(const char* path) {
    try {
        ResetState();

        _filepath = path;

        // 1. Read entire file into memory
        std::ifstream file(_filepath, std::ios::binary | std::ios::ate);
        if (!file.is_open()) {
            Log("Error: Unable to open Cartridge file.");
            return false;
        }

        std::streamsize size = file.tellg();
        if (size < INES_HEADER_SIZE) {
            Log(std::format("Error: Cartridge file too small. Need at least {} bytes.", INES_HEADER_SIZE).c_str());
            return false;
        }

        file.seekg(0, std::ios::beg);
        _romData.resize(size);

        if (!file.read(reinterpret_cast<char*>(_romData.data()), size)) {
            Log("Error: Unable to read Cartridge file.");
            return false;
        }

        // 2. Parse Header
        std::memcpy(&_header, _romData.data(), INES_HEADER_SIZE);

        if (!_header.is_valid()) {
            Log("Error: Invalid iNES header.");
            return false;
        }

        // 3. Calculate offsets and extract data
        uint32_t offset = INES_HEADER_SIZE;

        // Handle Trainer
        if (_header.has_trainer()) {
            _trainer = std::span(_romData.data() + offset, TRAINER_SIZE);
            offset += TRAINER_SIZE;
        }

        // Extract PRG ROM
        size_t prgRomSize = _header.get_prg_rom_size();
        if (offset + prgRomSize > _romData.size()) {
            Log("Error: PRG ROM size exceeds file size.");
            return false;
        }
        _prgRom.assign(_romData.begin() + offset, _romData.begin() + offset + prgRomSize);
        offset += prgRomSize;

        // Extract CHR ROM (if present)
        size_t chrRomSize = _header.get_chr_rom_size();
        if (chrRomSize > 0) {
            if (offset + chrRomSize > _romData.size()) {
                Log("Error: CHR ROM size exceeds file size.");
                return false;
            }
            _chrRom.assign(_romData.begin() + offset, _romData.begin() + offset + chrRomSize);
            Log(std::format("Info: Loaded {} bytes of CHR-ROM.", chrRomSize).c_str());
        }

        // Allocate RAM regions
        AllocateMemory();

        // Initialize mapper
        if (!InitializeMapper()) {
            return false;
        }

        _isLoaded = true;
        LogDiagnostics();
        return true;
    }
    catch (const std::exception& e) {
        Log(std::format("Error loading Cartridge: {}", e.what()).c_str());
        return false;
    }
}

void Cartridge::AllocateMemory() {
    // Allocate PRG-RAM
    size_t prgRamSize = _header.get_prg_ram_size();
    if (prgRamSize > 0) {
        _prgRam.assign(prgRamSize, 0x00);
        Log(std::format("Info: Allocated {} bytes of PRG-RAM.", prgRamSize).c_str());
    }

    // Allocate CHR-RAM if no CHR-ROM
    if (_chrRom.empty()) {
        size_t chrRamSize = _header.get_chr_ram_size();
        if (chrRamSize == 0) {
            chrRamSize = EIGHT_KB; // Default to 8KB
        }
        _chrRam.assign(chrRamSize, 0x00);
        Log(std::format("Info: Allocated {} bytes of CHR-RAM.", chrRamSize).c_str());
    }
}

bool Cartridge::InitializeMapper() {
    uint8_t mapperID = _header.get_mapper_number();

    if (!nes::MapperFactory::IsSupported(mapperID)) {
        Log(std::format("Unsupported mapper: {:d}", mapperID).c_str());
        return false;
    }

    uint8_t prgBanks = _header.get_prg_rom_size() / SIXTEEN_KB;
    uint8_t chrBanks = _header.get_chr_rom_size() / EIGHT_KB;

    _mapper = nes::MapperFactory::CreateMapper(mapperID, prgBanks, chrBanks);

    // Determine initial mirror mode from header
    MirrorMode initialMirror;
    if (_header.is_four_screen_mode()) {
        initialMirror = MirrorMode::FourScreen;
    }
    else if (_header.is_vertical_mirroring()) {
        initialMirror = MirrorMode::Vertical;
    }
    else {
        initialMirror = MirrorMode::Horizontal;
    }

    _mapper->SetInitalMapper(initialMirror);
    return true;
}

bool Cartridge::CpuRead(uint16_t addr, uint8_t& data) {
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;
    MemoryRegion region = MemoryRegion::None;

    // Ask mapper to translate the address
    if (_mapper->CpuMapRead(addr, mappedAddr, region)) {
        switch (region) {
        case MemoryRegion::PrgRom:
            if (mappedAddr < _prgRom.size()) {
                data = _prgRom[mappedAddr];
                return true;
            }
            Log(std::format("CPU read out of bounds - PRG-ROM addr: 0x{:04X}, mapped: 0x{:08X}", addr, mappedAddr).c_str());
            break;

        case MemoryRegion::PrgRam:
            if (mappedAddr < _prgRam.size()) {
                data = _prgRam[mappedAddr];
                return true;
            }
            Log(std::format("CPU read out of bounds - PRG-RAM addr: 0x{:04X}, mapped: 0x{:08X}", addr, mappedAddr).c_str());
            break;

        default:
            break;
        }
    }

    return false;
}

bool Cartridge::CpuWrite(uint16_t addr, uint8_t data) {
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;
    MemoryRegion region = MemoryRegion::None;

    // Ask mapper to translate the address
    if (_mapper->CpuMapWrite(addr, mappedAddr, region, data)) {
        switch (region) {
        case MemoryRegion::PrgRam:
            if (mappedAddr < _prgRam.size()) {
                _prgRam[mappedAddr] = data;
                return true;
            }
            Log(std::format("CPU write out of bounds - PRG-RAM addr: 0x{:04X}, mapped: 0x{:08X}", addr, mappedAddr).c_str());
            break;

        case MemoryRegion::None:
            // Mapper handled it internally (register write, etc.)
            return true;

        default:
            break;
        }
    }

    return false;
}

bool Cartridge::PpuRead(uint16_t addr, uint8_t& data) {
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;
    MemoryRegion region = MemoryRegion::None;

    // Ask mapper to translate the address
    if (_mapper->PpuMapRead(addr, mappedAddr, region)) {
        switch (region) {
        case MemoryRegion::ChrRom:
            if (mappedAddr < _chrRom.size()) {
                data = _chrRom[mappedAddr];
                return true;
            }
            Log(std::format("PPU read out of bounds - CHR-ROM addr: 0x{:04X}, mapped: 0x{:08X}", addr, mappedAddr).c_str());
            break;

        case MemoryRegion::ChrRam:
            if (mappedAddr < _chrRam.size()) {
                data = _chrRam[mappedAddr];
                return true;
            }
            Log(std::format("PPU read out of bounds - CHR-RAM addr: 0x{:04X}, mapped: 0x{:08X}", addr, mappedAddr).c_str());
            break;

        default:
            break;
        }
    }

    return false;
}

bool Cartridge::PpuWrite(uint16_t addr, uint8_t data) {
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;
    MemoryRegion region = MemoryRegion::None;

    // Ask mapper to translate the address
    if (_mapper->PpuMapWrite(addr, mappedAddr, region)) {
        switch (region) {
        case MemoryRegion::ChrRam:
            if (mappedAddr < _chrRam.size()) {
                _chrRam[mappedAddr] = data;
                return true;
            }
            Log(std::format("PPU write out of bounds - CHR-RAM addr: 0x{:04X}, mapped: 0x{:08X}", addr, mappedAddr).c_str());
            break;

        default:
            break;
        }
    }

    return false;
}
