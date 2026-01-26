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
    _chrRom.clear();
    _filepath.clear();

    // IMPORTANT: Spans MUST be reset if the owning vector is cleared 
    // to avoid dangling pointers.
    _prgRom = std::span<uint8_t>();
    _trainer = std::span<uint8_t>();

    // Use memset or brace initialization to ensure no garbage data remains
    std::memset(&_header, 0, sizeof(INESHeader));

    Log("Cartridge has been reset.");
}

#pragma region "Cartridge Callback Setup"
void Cartridge::SetDiagnosticLogCallback(DiagnosticLogCallback callback)
{
    if (callback != nullptr) {
        _diagnosticCallback = callback;
    }
    else {
        _diagnosticCallback = &DummyLogger;
    }
}
#pragma endregion

void Cartridge::Log(const char* msg)
{
    if (_diagnosticCallback && _loggingEnabled) _diagnosticCallback(msg);
}

void Cartridge::LogDiagnostics() {
    Log("");
    Log("===============================================================");
    Log("          MODERN CARTRIDGE LOADER - DIAGNOSTIC REPORT          ");
    Log("===============================================================");
    Log("");

    std::string filename = _filepath.filename().string();
    Log(std::format("Filename:        {}", filename).c_str());
    Log(std::format("File Size:       {} bytes", _romData.size()).c_str());

    // Format type
    const char* format = _header.is_nes2_format() ? "NES 2.0" : "iNES 1.0";
    Log(std::format("Format:          {}", format).c_str());

    // Mapper
    uint16_t mapperNum = _header.get_mapper_number();
    Log(std::format("Mapper:          {} (0x{:03X})", mapperNum, mapperNum).c_str());

    if (_header.is_nes2_format()) {
        uint8_t submapper = _header.get_submapper();
        if (submapper > 0) {
            Log(std::format("Submapper:       {}", submapper).c_str());
        }
    }

    Log("");
    Log("--- PRG (Program) Memory ---");

    // PRG ROM
    size_t prgSize = _header.get_prg_rom_size();
    Log(std::format("PRG ROM Size:    {} bytes ({} KB)", prgSize, prgSize / 1024).c_str());
    Log(std::format("PRG ROM Actual:  {} bytes", _prgRom.size()).c_str());

    // PRG-RAM (volatile work RAM)
    size_t prgRamSize = _header.get_prg_ram_size();
    if (prgRamSize > 0) {
        Log(std::format("PRG-RAM Size:    {} bytes ({} KB) [Volatile]", prgRamSize, prgRamSize / 1024).c_str());
    }
    else {
        Log("PRG-RAM Size:    0 bytes (None)");
    }

    // PRG-NVRAM (battery-backed save RAM)
    if (_header.is_nes2_format()) {
        size_t prgNvramSize = _header.get_prg_nvram_size();
        if (prgNvramSize > 0) {
            Log(std::format("PRG-NVRAM Size:  {} bytes ({} KB) [Battery-Backed]", prgNvramSize, prgNvramSize / 1024).c_str());
        }
        else {
            Log("PRG-NVRAM Size:  0 bytes (None)");
        }
    }
    else {
        // iNES 1.0 doesn't distinguish RAM/NVRAM, just show if battery is present
        if (_header.has_battery_backed_ram() && prgRamSize > 0) {
            Log(std::format("PRG-RAM:         {} bytes ({} KB) [Battery-Backed]", prgRamSize, prgRamSize / 1024).c_str());
        }
    }

    Log("");
    Log("--- CHR (Graphics) Memory ---");

    // CHR ROM/RAM
    size_t chrRomSize = _header.get_chr_rom_size();
    if (chrRomSize > 0) {
        Log(std::format("CHR ROM Size:    {} bytes ({} KB)", chrRomSize, chrRomSize / 1024).c_str());
        Log(std::format("CHR-ROM Size: {} bytes, CHR banks: {}", _chrRom.size(), _mapper->GetChrBanks()).c_str());
        Log("CHR Type:        ROM (Read-Only)");
    }
    else {
        Log("CHR ROM Size:    0 bytes (None)");
    }

    // CHR-RAM (volatile graphics RAM)
    size_t chrRamSize = _header.get_chr_ram_size();
    if (chrRamSize > 0) {
        Log(std::format("CHR-RAM Size:    {} bytes ({} KB) [Volatile]", chrRamSize, chrRamSize / 1024).c_str());
        if (chrRomSize == 0) {
            Log("CHR Type:        RAM (Read/Write)");
        }
    }
    else if (chrRomSize == 0) {
        Log("CHR-RAM Size:    0 bytes (None)");
    }

    // CHR-NVRAM (battery-backed graphics RAM - rare)
    if (_header.is_nes2_format()) {
        size_t chrNvramSize = _header.get_chr_nvram_size();
        if (chrNvramSize > 0) {
            Log(std::format("CHR-NVRAM Size:  {} bytes ({} KB) [Battery-Backed]", chrNvramSize, chrNvramSize / 1024).c_str());
        }
    }

    Log(std::format("CHR Buffer Size: {} bytes (Allocated)", _chrRom.size()).c_str());

    Log("");
    Log("--- Memory Summary ---");

    // Total ROM
    size_t totalRom = prgSize + chrRomSize;
    Log(std::format("Total ROM:       {} bytes ({} KB)", totalRom, totalRom / 1024).c_str());

    // Total RAM (volatile)
    size_t totalRam = prgRamSize + chrRamSize;
    if (totalRam > 0) {
        Log(std::format("Total RAM:       {} bytes ({} KB) [Volatile]", totalRam, totalRam / 1024).c_str());
    }
    else {
        Log("Total RAM:       0 bytes (None)");
    }

    // Total NVRAM (battery-backed)
    if (_header.is_nes2_format()) {
        size_t totalNvram = _header.get_prg_nvram_size() + _header.get_chr_nvram_size();
        if (totalNvram > 0) {
            Log(std::format("Total NVRAM:     {} bytes ({} KB) [Battery-Backed]", totalNvram, totalNvram / 1024).c_str());
        }
        else {
            Log("Total NVRAM:     0 bytes (None)");
        }
    }

    Log("");
    Log("--- Cartridge Configuration ---");

    // Mirroring
    if (_header.is_four_screen_mode()) {
        Log("Mirroring:       Four-Screen (Extra VRAM)");
    }
    else if (_header.is_vertical_mirroring()) {
        Log("Mirroring:       Vertical");
    }
    else {
        Log("Mirroring:       Horizontal");
    }

    // Battery
    Log(std::format("Battery:         {}", _header.has_battery_backed_ram() ? "Yes (Save RAM Present)" : "No").c_str());

    // Trainer
    Log(std::format("Trainer:         {}", _header.has_trainer() ? "Yes" : "No").c_str());
    if (_header.has_trainer()) {
        Log(std::format("Trainer Size:    {} bytes", _trainer.size()).c_str());
    }

    // TV System
    const char* tvSystem = "NTSC";
    switch (_header.get_tv_system()) {
    case INESHeader::TVSystem::NTSC: tvSystem = "NTSC (60 Hz)"; break;
    case INESHeader::TVSystem::PAL: tvSystem = "PAL (50 Hz)"; break;
    case INESHeader::TVSystem::Dual: tvSystem = "Dual (NTSC/PAL)"; break;
    case INESHeader::TVSystem::Dendy: tvSystem = "Dendy"; break;
    }
    Log(std::format("TV System:       {}", tvSystem).c_str());

    // PRG ROM dump
    if (_prgRom.size() >= 16) {
        Log("");
        Log("--- PRG ROM Analysis ---");
        Log("PRG ROM - First 16 bytes:");
        std::string first16 = "";
        for (int i = 0; i < 16; ++i) {
            first16 += std::format("{:02X} ", _prgRom[i]);
        }
        Log(first16.c_str());

        Log("PRG ROM - Last 16 bytes (vectors):");
        std::string last16 = "";
        for (size_t i = _prgRom.size() - 16; i < _prgRom.size(); ++i) {
            last16 += std::format("{:02X} ", _prgRom[i]);
        }
        Log(last16.c_str());

        // Decode vectors
        uint8_t nmiLo = _prgRom[_prgRom.size() - 6];
        uint8_t nmiHi = _prgRom[_prgRom.size() - 5];
        uint16_t nmiVec = (static_cast<uint16_t>(nmiHi) << 8) | nmiLo;

        uint8_t rstLo = _prgRom[_prgRom.size() - 4];
        uint8_t rstHi = _prgRom[_prgRom.size() - 3];
        uint16_t rstVec = (static_cast<uint16_t>(rstHi) << 8) | rstLo;

        uint8_t irqLo = _prgRom[_prgRom.size() - 2];
        uint8_t irqHi = _prgRom[_prgRom.size() - 1];
        uint16_t irqVec = (static_cast<uint16_t>(irqHi) << 8) | irqLo;

        Log(std::format("NMI Vector:      ${:04X}", nmiVec).c_str());
        Log(std::format("Reset Vector:    ${:04X}", rstVec).c_str());
        Log(std::format("IRQ Vector:      ${:04X}", irqVec).c_str());
    }

    Log("");
    Log("===============================================================");
}

#pragma region "Cartridge Propertys"
bool Cartridge::IsLoaded() const { return _isLoaded; }
uint8_t Cartridge::MapperID() const { return _header.get_mapper_number(); }
uint8_t Cartridge::PrgBanks() const { return _header.prg_rom_size; }
uint8_t Cartridge::ChrBanks() const { return (_header.chr_rom_size == 0 ? 1 : _header.chr_rom_size); } // 0 means CHR-RAM
MirrorMode Cartridge::GetMirrorMode() const {
    if (_mapper != nullptr) {
        MirrorMode mapperMirror = _mapper->GetMirrorMode();
        if (mapperMirror != MirrorMode::Hardware) {
            return mapperMirror;
        }
    }

    if (_header.is_four_screen_mode()) {
        // Some implementations define MirrorMode::FourScreen 
        // to represent extra VRAM on the cartridge.
        return MirrorMode::FourScreen;
    }
    else if (_header.is_vertical_mirroring()) {
        return MirrorMode::Vertical;
    }
    else {
        return MirrorMode::Horizontal;
    }
}
bool Cartridge::HasBattery() const { return _header.has_battery_backed_ram(); }
MapperBase* Cartridge::GetMapper() const {
    if (_mapper != nullptr) {
        return _mapper.get();
    }
    return nullptr;
}

void Cartridge::EnableLogging(bool enable) {
    _loggingEnabled = enable;
}
#pragma endregion

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

        // 3. Calculate offsets and slice
        uint32_t offset = INES_HEADER_SIZE;

        // Handle Trainer
        if (_header.has_trainer()) {
            _trainer = std::span(_romData.data() + offset, TRAINER_SIZE);
            offset += TRAINER_SIZE;
        }

        // Extract PRG ROM (use NES 2.0 aware size getter)
        size_t prgSize = _header.get_prg_rom_size();
        if (offset + prgSize > _romData.size()) {
            Log("Error: PRG ROM size exceeds file size.");
            return false;
        }
        _prgRom = std::span(_romData.data() + offset, prgSize);
        offset += prgSize;

        // Extract CHR ROM/RAM (use NES 2.0 aware size getter)
        size_t chrRomSize = _header.get_chr_rom_size();
        if (chrRomSize == 0) {
            // CHR-RAM: Allocate based on header info
            size_t chrRamSize = _header.get_chr_ram_size();
            _chrRom.assign(chrRamSize, 0);
            Log(std::format("Info: Allocated {} bytes of CHR-RAM.", chrRamSize).c_str());
        }
        else {
            // CHR-ROM: Copy from file
            if (offset + chrRomSize > _romData.size()) {
                Log("Error: CHR ROM size exceeds file size.");
                return false;
            }
            _chrRom.assign(_romData.begin() + offset, _romData.begin() + offset + chrRomSize);
        }

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

bool Cartridge::InitializeMapper() {
    uint8_t mapperID = _header.get_mapper_number();

    if (nes::MapperFactory::IsSupported(mapperID)) {
        //_mapper = nes::MapperFactory::CreateMapper(mapperID, _header.prg_rom_size, _header.chr_rom_size);
        uint8_t prgBanks = _header.get_prg_rom_size() / SIXTEEN_KB; // 16KB units
        uint8_t chrBanks = _header.get_chr_rom_size() / EIGHT_KB;   // 8KB units
        //if (chrBanks == 0) chrBanks = 1; // CHR-RAM fallback
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

        // Set both initial and current mirror mode
        _mapper->SetInitalMapper(initialMirror);
        return true;
    }
    else {
        Log(std::format("Unsupported mapper: {:d}", mapperID).c_str());
        return false;
    }
}

//void Cartridge::Clock() {
//    if (_mapper) {
//        _mapper->Clock();  // why did i have a clock in here?
//    }
//}

bool Cartridge::CpuRead(uint16_t addr, uint8_t& data) {
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;
    // The mapper may update 'data' directly if it's an internal RAM read
    if (_mapper->CpuMapRead(addr, mappedAddr, data)) {

        // If the mapper returned the sentinel, it handled the 'data' itself (e.g., WRAM)
        if (mappedAddr == 0xFFFFFFFF) {
            return true;
        }

        // Otherwise, it's a standard PRG-ROM read.
        // _prgRom can be a std::vector<uint8_t> or std::span<uint8_t>
        if (mappedAddr < _prgRom.size()) {
            data = _prgRom[mappedAddr];
            return true;
        }
    }

    return false;
}

/*
bool Cartridge::CpuWrite(uint16_t addr, uint8_t data) {
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;

    // Let mapper handle it
    bool mapperReturn = _mapper->CpuMapWrite(addr, mappedAddr, data);

    // If mapper returns true OR address is in mapper space, consider it handled
    if (mapperReturn || (addr >= 0x8000 && addr <= 0xFFFF)) {
        return true;  // Mapper handled it (either as memory write or register)
    }

    return false;
}
*/

bool Cartridge::CpuWrite(uint16_t addr, uint8_t data) {
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;
    // Mapper handles writes to its internal registers or WRAM
    if (_mapper->CpuMapWrite(addr, mappedAddr, data)) {

        // Sentinel check: Mapper handled the write (Register update or WRAM)
        if (mappedAddr == 0xFFFFFFFF) {
            return true;
        }

        // If your architecture supports writing back to a PRG-RAM buffer 
        // managed by the Cartridge class (instead of inside the Mapper), 
        // you would handle that here.
        return true;
    }

    return false;
}

bool Cartridge::PpuRead(uint16_t addr, uint8_t& data) {
    // Basic state check
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;
    // Ask the mapper where this PPU address resides in the CHR data
    if (_mapper->PpuMapRead(addr, mappedAddr)) {
        if (mappedAddr < _chrRom.size()) {
            data = _chrRom[mappedAddr];
            return true;
        }
        else {
            data = 0xFF; // TEMP: fill invalid reads with 0xFF to see difference
            Log(std::format("PPU read mappedAddr out of bounds: {:X}", mappedAddr).c_str());
            /*
                Reminder Zelda 2 blows passed the range, needs to be looked at eventually.
            */
        }
    }

    return false;
}

bool Cartridge::PpuWrite(uint16_t addr, uint8_t data) {
    if (!_isLoaded || _mapper == nullptr) return false;

    uint32_t mappedAddr = 0;
    // Ask the mapper if this address is writable (e.g., CHR-RAM)
    if (_mapper->PpuMapWrite(addr, mappedAddr)) {
        if (mappedAddr < _chrRom.size()) {
            _chrRom[mappedAddr] = data;
            return true;
        }
    }

    return false;
}
