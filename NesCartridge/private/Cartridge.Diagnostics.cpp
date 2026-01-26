#include "../core/Cartridge.h"

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

void Cartridge::SetDiagnosticLogCallback(DiagnosticLogCallback callback)
{
    if (callback != nullptr) {
        _diagnosticCallback = callback;
    }
    else {
        _diagnosticCallback = &DummyLogger;
    }
}

void Cartridge::Log(const char* msg)
{
    if (_diagnosticCallback && _loggingEnabled) _diagnosticCallback(msg);
}
