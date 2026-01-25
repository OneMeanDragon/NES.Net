#pragma once

#include <vector>
#include <cstdint>
#include <format>
#include <filesystem>
#include <fstream>

class MapperInterfaceAPI {
public:
    ~MapperInterfaceAPI() = default;
public:
    void ScanlineCounter() {}
    uint8_t Number() { return 0; }
};

// Forward declaration - adjust this based on your actual CartridgeInterfaceAPI
class CartridgeInterfaceAPI {
protected:
    MapperInterfaceAPI _mapper = MapperInterfaceAPI();
public:
    virtual ~CartridgeInterfaceAPI() = default;
    virtual uint8_t CpuRead(uint16_t addr, uint8_t* data = nullptr) = 0;
    virtual void CpuWrite(uint16_t addr, uint8_t data) = 0;
    virtual uint8_t PpuRead(uint16_t addr, uint8_t* data = nullptr) = 0;
    virtual void PpuWrite(uint16_t addr, uint8_t data) = 0;
    virtual MapperInterfaceAPI GetMapper() const = 0;
    virtual uint8_t GetMirrorMode() const = 0;
    virtual bool HasBatteryBackedRAM() const = 0;
};

class DiagnosticCartridge : public CartridgeInterfaceAPI {
private:
    std::vector<uint8_t> prgRom;
    std::vector<uint8_t> chrRom;
    uint8_t mapper;

public:
    DiagnosticCartridge() : mapper(0) {
        // Initialize with 32KB PRG ROM and 8KB CHR ROM
        prgRom.resize(32768, 0);
        chrRom.resize(8192, 0);
    }

    void LoadTestProgram(const std::vector<uint8_t>& program, uint16_t startAddr = 0x8000) {
        for (size_t i = 0; i < program.size() && (startAddr - 0x8000 + i) < prgRom.size(); i++) {
            prgRom[startAddr - 0x8000 + i] = program[i];
        }
    }

    bool LoadFromFile(const std::string& filename) {
        std::string path = std::filesystem::current_path().string();
        std::filesystem::path _filepath = std::format("{}\\{}", path, filename);
        std::ifstream file(_filepath, std::ios::binary);// | std::ios::ate);
        if (!file.is_open()) {
            return false;
        }

        // Read iNES header
        char header[16]{ 0 };
        file.read(reinterpret_cast<char*>(header), 16);

        if (header[0] != 'N' || header[1] != 'E' || header[2] != 'S' || header[3] != 0x1A) {
            return false; // Not a valid NES file
        }

        uint8_t prgBanks = header[4];
        uint8_t chrBanks = header[5];

        // Resize ROM
        prgRom.resize(prgBanks * 16384);
        chrRom.resize(chrBanks * 8192);

        // Skip trainer if present
        if (header[6] & 0x04) {
            file.seekg(512, std::ios::cur);
        }

        // Read PRG ROM
        file.read(reinterpret_cast<char*>(prgRom.data()), prgRom.size());

        // Read CHR ROM
        if (chrBanks > 0) {
            file.read(reinterpret_cast<char*>(chrRom.data()), chrRom.size());
        }

        mapper = ((header[6] >> 4) & 0x0F) | (header[7] & 0xF0);

        return true;
    }

    uint8_t CpuRead(uint16_t addr, uint8_t* data) override {
        if (addr >= 0x8000) {
            if (data) {
                *data = prgRom[(addr - 0x8000) % prgRom.size()];
                return *data;
            }
            else {
                return prgRom[(addr - 0x8000) % prgRom.size()];
            }
        }
        return 0;
    }

    void CpuWrite(uint16_t addr, uint8_t data) override {
        // Most mappers don't allow writing to PRG ROM
        // Override this for specific mapper tests
    }

    uint8_t PpuRead(uint16_t addr, uint8_t* data) override {
        if (addr < 0x2000) {
            if (data) {
                *data = chrRom[addr % chrRom.size()];
                return *data;
            }
            else {
                return chrRom[addr % chrRom.size()];
            }
        }
        return 0;
    }

    void PpuWrite(uint16_t addr, uint8_t data) override {
        if (addr < 0x2000 && chrRom.size() > 0) {
            chrRom[addr % chrRom.size()] = data;
        }
    }

    MapperInterfaceAPI GetMapper() const override { return _mapper; }
    uint8_t GetMirrorMode() const override { return 0; } // Horizontal
    bool HasBatteryBackedRAM() const override { return false; }

    // Direct memory access for debugging
    const std::vector<uint8_t>& GetPrgRom() const { return prgRom; }
    const std::vector<uint8_t>& GetChrRom() const { return chrRom; }
};