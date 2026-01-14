#include "pch.h"
#include "../core/Cartridge.h"

#include <iostream>
#include <fstream>

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

bool Cartridge::Load(const char* path) {
    //try {
    //    _isLoaded = false;
    //    // In C++, the mapper reset/deletion would happen here
    //    // e.g., _mapper.reset();
    //
    //    // 1. Read entire file into memory
    //    std::ifstream file(path, std::ios::binary | std::ios::ate);
    //    if (!file.is_open()) return false;
    //
    //    std::streamsize size = file.tellg();
    //    if (size < INES_HEADER_SIZE) return false;
    //
    //    file.seekg(0, std::ios::beg);
    //    _romData.resize(size);
    //    if (!file.read(reinterpret_cast<char*>(_romData.data()), size)) {
    //        return false;
    //    }
    //
    //    // 2. Parse Header (direct copy into struct)
    //    std::memcpy(&_header, _romData.data(), INES_HEADER_SIZE);
    //
    //    if (!_header.is_valid()) {
    //        std::cerr << "Invalid iNES header" << std::endl;
    //        return false;
    //    }
    //
    //    // 3. Calculate offsets and Slice
    //    uint32_t offset = INES_HEADER_SIZE;
    //
    //    // Handle Trainer
    //    if (_header.has_trainer()) {
    //        _trainer = std::span(_romData.data() + offset, TRAINER_SIZE);
    //        offset += TRAINER_SIZE;
    //    }
    //
    //    // Extract PRG ROM
    //    uint32_t prgSize = _header.prg_rom_size * PRG_BANK_SIZE;
    //    if (offset + prgSize > _romData.size()) return false;
    //    _prgRom = std::span(_romData.data() + offset, prgSize);
    //    offset += prgSize;
    //
    //    // Extract CHR ROM/RAM
    //    if (_header.chr_rom_size == 0) {
    //        // CHR-RAM: Allocate 8KB of writable RAM
    //        _chrRom.assign(CHR_BANK_SIZE, 0);
    //    }
    //    else {
    //        // CHR-ROM: Copy from file into its own vector 
    //        // (Done so we can modify it if mappers require, or just span it)
    //        uint32_t chrSize = _header.chr_rom_size * CHR_BANK_SIZE;
    //        if (offset + chrSize > _romData.size()) return false;
    //
    //        _chrRom.assign(_romData.begin() + offset, _romData.begin() + offset + chrSize);
    //    }
    //
    //    // 4. Initialize Mapper (Logic goes here)
    //    // if (!InitializeMapper()) return false;
    //
    //    _isLoaded = true;
    //    return true;
    //
    //}
    //catch (const std::exception& e) {
    //    std::cerr << "Error loading ROM: " << e.what() << std::endl;
    //    return false;
    //}
    return true;
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

DLLEXPORT bool LoadRom(Cartridge* cart, const char* path) {
	bool result = cart->Load(path);
	if (result) cart->Log("ROM loaded successfully.");
	else cart->Log("Error: ROM load failed.");
	return result;
}

DLLEXPORT bool CartCpuRead(Cartridge* cart, uint16_t addr, uint8_t* data) {
	return cart->CpuRead(addr, *data);
}

#pragma endregion