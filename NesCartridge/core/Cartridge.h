#pragma once

#include <cstdint>
#include <vector>
#include <span>
#include <memory>
#include <filesystem>

#include "INESHeader.h"
#include "mappers/MapperBase.h"

// Diagnostics
#include "Diagnostics/DiagnosticHelpers.h"

// Memory region identifiers for mapper communication
enum class MemoryRegion : uint8_t {
    None = 0,
    PrgRom,
    PrgRam,
    ChrRom,
    ChrRam
};

class Cartridge {
private:
    std::filesystem::path _filepath{};

private:
    std::unique_ptr<MapperBase> _mapper = nullptr;

private:
    // File data
    std::vector<uint8_t> _romData{};      // Holds the entire file
    std::span<uint8_t> _trainer{};         // View into _romData

    // PRG memory (CPU address space)
    std::vector<uint8_t> _prgRom{};        // PRG-ROM (read-only)
    std::vector<uint8_t> _prgRam{};        // PRG-RAM (6000-7FFF, battery-backed or not)

    // CHR memory (PPU address space)
    std::vector<uint8_t> _chrRom{};        // CHR-ROM (read-only)
    std::vector<uint8_t> _chrRam{};        // CHR-RAM (writable)

private:
    INESHeader _header{ 0 };
    bool _isLoaded = false;

public: // Readonly properties
    bool IsLoaded() const;
    uint8_t MapperID() const;
    uint8_t PrgBanks() const;
    uint8_t ChrBanks() const;
    MirrorMode GetMirrorMode() const;
    bool HasBattery() const;
    MapperBase* GetMapper() const;

private:
    bool _loggingEnabled = false;
    static void __stdcall DummyLogger(const char* message) { /*static*/ }

public:
    DiagnosticLogCallback _diagnosticCallback = &DummyLogger;
    void EnableLogging(bool enable);
    bool LoggingEnabled() const { return _loggingEnabled; }

public:
    void SetDiagnosticLogCallback(DiagnosticLogCallback callback);

private:
    bool InitializeMapper();
    void AllocateMemory();

public:
    // Destructor - auto-saves battery RAM if present
    ~Cartridge();

    void Log(const char* msg);

public:
    bool Load(const char* path);
    bool CpuRead(uint16_t addr, uint8_t& data);
    bool CpuWrite(uint16_t addr, uint8_t data);
    bool PpuRead(uint16_t addr, uint8_t& data);
    bool PpuWrite(uint16_t addr, uint8_t data);

    // Reset mapper to initial state (soft reset)
    void Reset();

    // Battery-backed RAM save/load
    bool SaveBatteryRam();
    bool LoadBatteryRam();

private:
    void ResetState();
    void LogDiagnostics();
    std::filesystem::path GetSaveFilePath() const;
};