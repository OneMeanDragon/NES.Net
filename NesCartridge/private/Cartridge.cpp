#include "pch.h"
#include "../core/Cartridge.h"

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

bool Cartridge::Load(const char* path) {
    try {
        _isLoaded = false;
        // In C++, the mapper reset/deletion would happen here
        // e.g., _mapper.reset();
    
        // 1. Read entire file into memory
        std::ifstream file(path, std::ios::binary | std::ios::ate);
        if (!file.is_open()) {
            Log("Error: Unable to open Cartridge file.");
            return false;
        }

        std::streamsize size = file.tellg();
        if (size < INES_HEADER_SIZE) {
            std::ostringstream out;
            out << "Error: Cartridge file too small: ";
            out << INES_HEADER_SIZE;
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
            // (Done so we can modify it if mappers require, or just span it)
            uint32_t chrSize = _header.chr_rom_size * CHR_BANK_SIZE;
            if (offset + chrSize > _romData.size()) return false;
    
            _chrRom.assign(_romData.begin() + offset, _romData.begin() + offset + chrSize);
        }
    
        // 4. Initialize Mapper (Logic goes here)
        // if (!InitializeMapper()) return false;
    
        _isLoaded = true;
        LogDiagnostics();
        return true;
    
    }
    catch (const std::exception& e) {
        std::ostringstream out;
        out << "Error loading Cartridge: ";
        out << e.what();
        Log(out.str().c_str());
        return false;
    }
}

bool Cartridge::CpuRead(uint16_t addr, uint8_t& data) {
	return true;
}

bool Cartridge::CpuWrite(uint16_t addr, uint8_t data)
{
	return true;
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
	bool result = cart->Load(path);
	if (result) cart->Log("ROM loaded successfully.");
	else cart->Log("Error: ROM load failed.");
	return result;
}

DLLEXPORT bool CartCpuRead(Cartridge* cart, uint16_t addr, uint8_t* data) {
	return cart->CpuRead(addr, *data);
}

#pragma endregion