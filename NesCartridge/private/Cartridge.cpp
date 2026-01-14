#include "../core/Cartridge.h"
#include "../core/mappers/MapperFactory.h"

#include <iostream>
#include <fstream>
#include <sstream>

#include <format>

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
    if (_diagnosticCallback) _diagnosticCallback(msg);
}

void Cartridge::LogDiagnostics() {
    Log("");
    Log("===============================================================");
    Log("          MODERN CARTRIDGE LOADER - DIAGNOSTIC REPORT          ");
    Log("===============================================================");
    Log("");

    std::string msg;
    msg = std::format("File Size:       {} bytes.", _romData.size());
    Log(msg.c_str());
    msg = std::format("Mapper:          {} (0x{:02X}).", _header.get_mapper_number(), _header.get_mapper_number());
    Log(msg.c_str());
    msg = std::format("PRG Banks:       {} * 16KB = {} KB.", _header.prg_rom_size, (_header.prg_rom_size * 16));
    Log(msg.c_str());
    msg = std::format("CHR Banks:       {} * 8KB = {} KB.", _header.chr_rom_size, (_header.chr_rom_size * 8));
    Log(msg.c_str());
    msg = std::format("CHR Type:        {}.", (_header.chr_rom_size == 0 ? "RAM" : "ROM"));
    Log(msg.c_str());
    msg = std::format("Mirroring:       {}.", (_header.is_vertical_mirroring() ? "Vertical" : "Horizontal"));
    Log(msg.c_str());
    msg = std::format("Battery:         {}.", _header.has_battery_backed_ram());
    Log(msg.c_str());
    msg = std::format("Trainer:         {}.", _header.has_trainer());
    Log(msg.c_str());
    msg = std::format("Format:          {}.", (_header.is_nes2_format() ? "iNES 2.0" : "iNES 1.0"));
    Log(msg.c_str());

    if (_prgRom.size() >= INES_HEADER_SIZE) {
        Log("");
        Log("PRG ROM - First 16 bytes:");

        std::string first16 = "";
        for (int i = 0; i < 16; ++i) {
            // Append formatted hex string
            first16 += std::format("{:02X} ", _prgRom[i]);
        }
        Log(first16.c_str());

        Log("PRG ROM - Last 16 bytes (vectors):");
        std::string last16 = "";
        for (size_t i = _prgRom.size() - 16; i < _prgRom.size(); ++i) {
            last16 += std::format("{:02X} ", _prgRom[i]);
        }
        Log(last16.c_str());

        // Decode reset vector
        // NES Reset vector is at $FFFC-$FFFD (the last 4th and 3rd bytes of PRG ROM)
        uint8_t rstLo = _prgRom[_prgRom.size() - 4];
        uint8_t rstHi = _prgRom[_prgRom.size() - 3];
        uint16_t rstVec = (static_cast<uint16_t>(rstHi) << 8) | rstLo;

        Log(std::format("Reset Vector:    ${:04X}", rstVec).c_str());
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
#pragma endregion

//bool Cartridge::Load(const char* path) {
//    try {
//        _isLoaded = false;
//        // In C++, the mapper reset/deletion would happen here
//        // e.g., _mapper.reset();
//    
//        // 1. Read entire file into memory
//        std::ifstream file(path, std::ios::binary | std::ios::ate);
//        if (!file.is_open()) {
//            Log("Error: Unable to open Cartridge file.");
//            return false;
//        }
//
//        std::streamsize size = file.tellg();
//        if (size < INES_HEADER_SIZE) {
//            std::ostringstream out;
//            out << "Error: Cartridge file too small: ";
//            out << INES_HEADER_SIZE;
//            Log(out.str().c_str());
//            return false;
//        }
//
//        file.seekg(0, std::ios::beg);
//        _romData.resize(size);
//
//        if (!file.read(reinterpret_cast<char*>(_romData.data()), size)) {
//            Log("Error: Unable to read Cartridge file.");
//            return false;
//        }
//    
//        // 2. Parse Header (direct copy into struct)
//        std::memcpy(&_header, _romData.data(), INES_HEADER_SIZE);
//    
//        if (!_header.is_valid()) {
//            Log("Error: Invalid iNES header.");
//            return false;
//        }
//    
//        // 3. Calculate offsets and Slice
//        uint32_t offset = INES_HEADER_SIZE;
//    
//        // Handle Trainer
//        if (_header.has_trainer()) {
//            _trainer = std::span(_romData.data() + offset, TRAINER_SIZE);
//            offset += TRAINER_SIZE;
//        }
//    
//        // Extract PRG ROM
//        uint32_t prgSize = _header.prg_rom_size * PRG_BANK_SIZE;
//        if (offset + prgSize > _romData.size()) return false;
//        _prgRom = std::span(_romData.data() + offset, prgSize);
//        offset += prgSize;
//    
//        // Extract CHR ROM/RAM
//        //if (_header.chr_rom_size == 0) {
//        //    // CHR-RAM: Allocate 8KB of writable RAM
//        //    _chrRom.assign(CHR_BANK_SIZE, 0);
//        //}
//        //else {
//        //    // CHR-ROM: Copy from file into its own vector 
//        //    // (Done so we can modify it if mappers require, or just span it)
//        //    uint32_t chrSize = _header.chr_rom_size * CHR_BANK_SIZE;
//        //    if (offset + chrSize > _romData.size()) return false;
//        //
//        //    _chrRom.assign(_romData.begin() + offset, _romData.begin() + offset + chrSize);
//        //}
//        if (_header.chr_rom_size == 0) {
//            // CHR-RAM: Allocate 8KB of writable RAM
//            _chrRom.assign(CHR_BANK_SIZE, 0);
//        }
//        else {
//            // CHR-ROM: Copy from file into its own vector 
//            // (Done so we can modify it if mappers require, or just span it)
//            uint32_t chrSize = _header.chr_rom_size * CHR_BANK_SIZE;
//            if (offset + chrSize > _romData.size()) return false;
//
//            _chrRom.assign(_romData.begin() + offset, _romData.begin() + offset + chrSize);
//        }
//
//        // Log CHR buffer size for diagnostics
//        Log(std::format("Debug: CHR buffer size = {} bytes (chr_rom_size field = {})", _chrRom.size(), _header.chr_rom_size).c_str());
//
//    
//        // Initialize mapper
//        if (!InitializeMapper()) {
//            return false;
//        }
//    
//        _isLoaded = true;
//        LogDiagnostics();
//        return true;
//    
//    }
//    catch (const std::exception& e) {
//        std::ostringstream out;
//        out << "Error loading Cartridge: ";
//        out << e.what();
//        Log(out.str().c_str());
//        return false;
//    }
//}
bool Cartridge::Load(const char* path) {
    try {
        _isLoaded = false;
        // _mapper.reset(); // already handled elsewhere if needed

        // 1. Read entire file into memory
        std::ifstream file(path, std::ios::binary | std::ios::ate);
        if (!file.is_open()) {
            Log("Error: Unable to open Cartridge file.");
            return false;
        }

        std::streamsize size = file.tellg();
        if (size < INES_HEADER_SIZE) {
            std::ostringstream out;
            out << "Error: Cartridge file too small: " << INES_HEADER_SIZE;
            Log(out.str().c_str());
            return false;
        }

        file.seekg(0, std::ios::beg);
        _romData.resize(size);

        if (!file.read(reinterpret_cast<char*>(_romData.data()), size)) {
            Log("Error: Unable to read Cartridge file.");
            return false;
        }

        // 2. Parse Header (direct copy into struct)
        std::memcpy(&_header, _romData.data(), INES_HEADER_SIZE);

        if (!_header.is_valid()) {
            Log("Error: Invalid iNES header.");
            return false;
        }

        // 3. Calculate offsets and Slice
        uint32_t offset = INES_HEADER_SIZE;

        // Handle Trainer
        if (_header.has_trainer()) {
            _trainer = std::span(_romData.data() + offset, TRAINER_SIZE);
            offset += TRAINER_SIZE;
        }

        // Extract PRG ROM
        uint32_t prgSize = _header.prg_rom_size * PRG_BANK_SIZE;
        if (offset + prgSize > _romData.size()) return false;
        _prgRom = std::span(_romData.data() + offset, prgSize);
        offset += prgSize;

        // Extract CHR ROM/RAM
        if (_header.chr_rom_size == 0) {
            // CHR-RAM: Allocate 8KB of writable RAM
            _chrRom.assign(CHR_BANK_SIZE, 0);
        }
        else {
            // CHR-ROM: Copy from file into its own vector
            uint32_t chrSize = _header.chr_rom_size * CHR_BANK_SIZE;
            if (offset + chrSize > _romData.size()) return false;

            _chrRom.assign(_romData.begin() + offset, _romData.begin() + offset + chrSize);
        }

        // Initialize mapper
        if (!InitializeMapper()) {
            return false;
        }

        _isLoaded = true;

        // Diagnostic: report CHR buffer and dump a few regions important for pattern tables
        Log(std::format("Debug: CHR buffer size = {} bytes (chr_rom_size field = {})", _chrRom.size(), _header.chr_rom_size).c_str());

        // Dump first 32 bytes (0x0000..0x001F) and the 0x1240..0x127F range if present
        {
            std::string s = "Debug: CHR @0x0000..0x001F:";
            size_t limit = std::min<size_t>(_chrRom.size(), 0x20);
            for (size_t i = 0; i < limit; ++i) {
                s += std::format(" {:02X}", _chrRom[i]);
            }
            Log(s.c_str());
        }

        const uint32_t probeAddr = 0x1240;
        const uint32_t probeLen = 64;
        if (probeAddr < _chrRom.size()) {
            std::string s = std::format("Debug: CHR dump @0x{0:04X}..0x{1:04X}:", probeAddr, probeAddr + probeLen - 1);
            size_t limit = std::min<size_t>(_chrRom.size(), probeAddr + probeLen);
            for (size_t i = probeAddr; i < limit; ++i) {
                s += std::format(" {:02X}", _chrRom[i]);
            }
            Log(s.c_str());
        }
        else {
            Log(std::format("Debug: CHR probe @0x{0:04X} out-of-range (size={1})", probeAddr, _chrRom.size()).c_str());
        }

        LogDiagnostics();
        return true;
    }
    catch (const std::exception& e) {
        std::ostringstream out;
        out << "Error loading Cartridge: " << e.what();
        Log(out.str().c_str());
        return false;
    }
}

bool Cartridge::InitializeMapper() {
    uint8_t mapperID = _header.get_mapper_number();

    if (nes::MapperFactory::IsSupported(mapperID)) {
        // Create the mapper and store it in our unique_ptr
        _mapper = nes::MapperFactory::CreateMapper(mapperID, _header.prg_rom_size, _header.chr_rom_size);

        std::string name = nes::MapperFactory::GetMapperName(mapperID);
        Log(std::format("Loaded: {}", name).c_str());
        return true;
    }
    else {
        Log(std::format("Unsupported mapper: {:d}", mapperID).c_str());
        return false;
    }
}

void Cartridge::Clock() {
    if (_mapper) {
        _mapper->Clock();  // Let mapper handle its own timing
    }
}

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
        // Bounds check against the loaded CHR data
        if (mappedAddr < _chrRom.size()) {
            data = _chrRom[mappedAddr];
            return true;
        }
    }

    return false;
}
//bool Cartridge::PpuRead(uint16_t addr, uint8_t& data) {
//    // Basic state check
//    if (!_isLoaded || _mapper == nullptr) {
//        Log(std::format("PpuRead: ignored - not loaded or no mapper (addr=0x{:04X})", addr).c_str());
//        return false;
//    }
//
//    uint32_t mappedAddr = 0;
//    bool mapped = _mapper->PpuMapRead(addr, mappedAddr);
//
//    if (!mapped) {
//        Log(std::format("PpuRead: mapper did NOT map addr 0x{:04X}", addr).c_str());
//        return false;
//    }
//
//    Log(std::format("PpuRead: addr=0x{:04X} -> mappedAddr=0x{:06X} (CHR size={} bytes)",
//        addr, mappedAddr, _chrRom.size()).c_str());
//
//    if (mappedAddr < _chrRom.size()) {
//        data = _chrRom[mappedAddr];
//        Log(std::format("PpuRead: returning 0x{:02X} for PPU addr 0x{:04X} (mapped 0x{:06X})",
//            data, addr, mappedAddr).c_str());
//        return true;
//    }
//    else {
//        Log(std::format("PpuRead: mappedAddr 0x{:06X} out of bounds (CHR size {}) for PPU addr 0x{:04X}",
//            mappedAddr, _chrRom.size(), addr).c_str());
//    }
//
//    return false;
//}

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

#pragma region "Exported Cartridge Functions"
DLLEXPORT void CartridgeSetDiagnosticLogCallback(Cartridge* cart, DiagnosticLogCallback callback) {
    if (cart && callback) {
        cart->SetDiagnosticLogCallback(callback);
        cart->Log("Info: Diagnostic Log Callback attached successfully.");
    }
    else if (cart == nullptr) {
        if (callback) callback("Error: Cartridge instance is nullptr.");
    }
}

DLLEXPORT Cartridge* CreateCartridge() {
    return new Cartridge();
}

DLLEXPORT Cartridge* CreateCartridgeDiag(DiagnosticLogCallback callback) {
    Cartridge* cart = new Cartridge();
    if (cart == nullptr) {
        if (callback) callback("Error: Unable to create Cartridge instance.");
        return nullptr;
    }
    cart->SetDiagnosticLogCallback(callback);
    cart->Log("Native Cartridge instance created.");
    return cart;
}

DLLEXPORT void DestroyCartridge(Cartridge* cart) { 
	delete cart; 
}

DLLEXPORT bool LoadCartridge(Cartridge* cart, const char* path) {
    if (cart && path) {
        bool result = cart->Load(path);
        if (result) cart->Log("ROM loaded successfully.");
        else cart->Log("Error: ROM load failed.");
        return result;
    }
    return false;
}

DLLEXPORT MirrorMode CartridgeGetMirrorMode(Cartridge* cart) {
    if (cart) return cart->GetMirrorMode();
    return MirrorMode::Hardware;
}

DLLEXPORT bool CartridgeIsLoaded(Cartridge* cart) {
    if (cart) return cart->IsLoaded();
    return false;
}

DLLEXPORT void CartridgeClock(Cartridge* cart) {
    if (cart) cart->Clock();
}

DLLEXPORT bool CartCpuRead(Cartridge* cart, uint16_t addr, uint8_t* data) {
    if (cart) return cart->CpuRead(addr, *data);
    return false;
}

DLLEXPORT bool CartCpuWrite(Cartridge* cart, uint16_t addr, uint8_t data) {
    if (cart) return cart->CpuWrite(addr, data);
    return false;
}

DLLEXPORT bool CartPpuRead(Cartridge* cart, uint16_t addr, uint8_t* data) {
    if (cart) return cart->PpuRead(addr, *data);
    return false;
}

DLLEXPORT bool CartPpuWrite(Cartridge* cart, uint16_t addr, uint8_t data) {
    if (cart) return cart->PpuWrite(addr, data);
    return false;
}


#pragma region "Exported MapperBase Functions"
// ============================= MAPPER RELATED Functionality =============================
DLLEXPORT MapperBase* CartridgeMapper(Cartridge* cart) {
    if (cart) return cart->GetMapper();
    return nullptr;
}
DLLEXPORT bool MapperIsIrqActive(MapperBase* mapper) {
    if (mapper) return mapper->IsIrqActive();
    return false;
}
DLLEXPORT void MapperClearIrq(MapperBase* mapper) {
    if (mapper) mapper->ClearIrq();
}
DLLEXPORT void MapperReset(MapperBase* mapper) {
    if (mapper) mapper->Reset();
}
DLLEXPORT MirrorMode MapperGetMirrorMode(MapperBase* mapper) {
    if (mapper) return mapper->GetMirrorMode();
    return MirrorMode::Hardware;
}
#pragma endregion

#pragma endregion