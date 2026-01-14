#pragma once

#include <cstdint>
#include <vector>
#include <span>

#include "INESHeader.h"

#define DLLEXPORT extern "C" __declspec(dllexport)

#pragma region "Callback Types"
typedef void(__stdcall* DiagnosticLogCallback)(const char* message);
#pragma endregion

class Cartridge {
private:
    uint8_t mapperID;
    std::vector<uint8_t> _romData;   // Holds the entire file
    std::span<uint8_t> _prgRom;      // View into _romData (zero-copy)
    std::vector<uint8_t> _chrRom;    // Vector because it may be ROM or RAM
    std::span<uint8_t> _trainer;     // View into _romData

    INESHeader _header;
    bool _isLoaded = false;

private:
    static void __stdcall DummyLogger(const char* message) {}
    DiagnosticLogCallback _diagnosticCallback = &DummyLogger;
public:
    void SetDiagnosticLogCallback(DiagnosticLogCallback callback);

public:
    void Log(const char* msg);

public:
    bool Load(const char* path);
    bool CpuRead(uint16_t addr, uint8_t& data);
    bool CpuWrite(uint16_t addr, uint8_t data);
};