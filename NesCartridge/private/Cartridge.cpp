#include "../core/Cartridge.h"
#include "../core/mappers/MapperFactory.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <format>

Cartridge::~Cartridge() {
    // Auto-save battery-backed RAM on destruction
    if (_isLoaded && HasBattery() && !_prgRam.empty()) {
        SaveBatteryRam();
    }
}

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

        // Auto-load battery-backed RAM if present
        if (HasBattery()) {
            LoadBatteryRam();
        }

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

void Cartridge::Reset() {
    if (_mapper) {
        _mapper->Reset();
        Log("Mapper reset to initial state.");
    }
}

std::filesystem::path Cartridge::GetSaveFilePath() const {
    if (_filepath.empty()) {
        return std::filesystem::path();
    }

    // Get the directory containing the ROM
    std::filesystem::path romDir = _filepath.parent_path();

    // Create "save" subdirectory path
    std::filesystem::path saveDir = romDir / "save";

    // Get ROM filename without extension and add .sav
    std::string saveFilename = _filepath.stem().string() + ".sav";

    return saveDir / saveFilename;
}

bool Cartridge::SaveBatteryRam() {
    if (!_isLoaded) {
        Log("Error: Cannot save battery RAM - cartridge not loaded.");
        return false;
    }

    if (!HasBattery()) {
        Log("Info: Cartridge does not have battery-backed RAM.");
        return true; // Not an error, just nothing to save
    }

    if (_prgRam.empty()) {
        Log("Warning: Battery flag set but no PRG-RAM allocated.");
        return true;
    }

    try {
        std::filesystem::path savePath = GetSaveFilePath();

        // Create save directory if it doesn't exist
        std::filesystem::path saveDir = savePath.parent_path();
        if (!std::filesystem::exists(saveDir)) {
            std::filesystem::create_directories(saveDir);
            Log(std::format("Created save directory: {}", saveDir.string()).c_str());
        }

        // Write PRG-RAM to file
        std::ofstream saveFile(savePath, std::ios::binary);
        if (!saveFile.is_open()) {
            Log(std::format("Error: Unable to create save file: {}", savePath.string()).c_str());
            return false;
        }

        saveFile.write(reinterpret_cast<const char*>(_prgRam.data()), _prgRam.size());

        if (!saveFile.good()) {
            Log(std::format("Error: Failed to write save file: {}", savePath.string()).c_str());
            return false;
        }

        Log(std::format("Battery RAM saved: {} ({} bytes)", savePath.string(), _prgRam.size()).c_str());
        return true;
    }
    catch (const std::exception& e) {
        Log(std::format("Error saving battery RAM: {}", e.what()).c_str());
        return false;
    }
}

bool Cartridge::LoadBatteryRam() {
    if (!_isLoaded) {
        Log("Error: Cannot load battery RAM - cartridge not loaded.");
        return false;
    }

    if (!HasBattery()) {
        return true; // Not an error, just nothing to load
    }

    if (_prgRam.empty()) {
        Log("Warning: Battery flag set but no PRG-RAM allocated.");
        return true;
    }

    try {
        std::filesystem::path savePath = GetSaveFilePath();

        // Check if save file exists
        if (!std::filesystem::exists(savePath)) {
            Log(std::format("Info: No save file found at: {}", savePath.string()).c_str());
            return true; // Not an error - new game
        }

        // Check file size matches PRG-RAM size
        size_t fileSize = std::filesystem::file_size(savePath);
        if (fileSize != _prgRam.size()) {
            Log(std::format("Warning: Save file size ({} bytes) doesn't match PRG-RAM size ({} bytes). Save file may be corrupt.",
                fileSize, _prgRam.size()).c_str());
            // Continue anyway and load what we can
        }

        // Read save file
        std::ifstream saveFile(savePath, std::ios::binary);
        if (!saveFile.is_open()) {
            Log(std::format("Error: Unable to open save file: {}", savePath.string()).c_str());
            return false;
        }

        // Read into PRG-RAM (only up to PRG-RAM size)
        size_t bytesToRead = std::min(fileSize, _prgRam.size());
        saveFile.read(reinterpret_cast<char*>(_prgRam.data()), bytesToRead);

        if (!saveFile.good() && !saveFile.eof()) {
            Log(std::format("Error: Failed to read save file: {}", savePath.string()).c_str());
            return false;
        }

        Log(std::format("Battery RAM loaded: {} ({} bytes)", savePath.string(), bytesToRead).c_str());
        return true;
    }
    catch (const std::exception& e) {
        Log(std::format("Error loading battery RAM: {}", e.what()).c_str());
        return false;
    }
}