#pragma once

#include <cstdint>
#include <vector>
#include <span>
#include <memory>

#include "INESHeader.h"
#include "mappers/MapperBase.h"

// Diagnostics
#include "Diagnostics/DiagnosticHelpers.h"

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif


class Cartridge {
private:
    std::unique_ptr<MapperBase> _mapper = nullptr;
private:
    std::vector<uint8_t> _romData{};   // Holds the entire file
    std::span<uint8_t> _prgRom{};      // View into _romData (zero-copy)
    std::vector<uint8_t> _chrRom{};    // Vector because it may be ROM or RAM
    std::span<uint8_t> _trainer{};     // View into _romData

private:
    INESHeader _header{ 0 };
    bool _isLoaded = false;

public: //readonly propertys
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
    // Destroy needs to know the callback
    DiagnosticLogCallback _diagnosticCallback = &DummyLogger;
    void EnableLogging(bool enable);
	bool LoggingEnabled() const { return _loggingEnabled; }
public:
    void SetDiagnosticLogCallback(DiagnosticLogCallback callback);

private:
    bool InitializeMapper();

public:
    void Log(const char* msg);

public:
    bool Load(const char* path);
    bool CpuRead(uint16_t addr, uint8_t& data);
    bool CpuWrite(uint16_t addr, uint8_t data);
    bool PpuRead(uint16_t addr, uint8_t& data);
    bool PpuWrite(uint16_t addr, uint8_t data);
    void Clock();
    
private:
    void ResetState();
    void LogDiagnostics();
};