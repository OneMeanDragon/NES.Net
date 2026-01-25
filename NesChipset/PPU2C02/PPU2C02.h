#pragma once

#include <cstdint>
#include "PPU2C02.Types.h"
#include "PPU2C02.Memory.h"
#include "PPU2C02.Registers.h"
#include "PPU2C02.Background.h"
#include "PPU2C02.Sprites.h"
#include "../Diagnostics/DiagnosticHelpers.h"

constexpr int16_t SCANLINE_MAX   = 261;
constexpr int16_t SCANLINE_START =  -1;
constexpr int16_t CYCLE_MAX      = 341;

// Forward declarations
class CartridgeInterfaceAPI;
class NESBus;

// Callback types
typedef void (*PixelCallback)(int x, int y, uint8_t r, uint8_t g, uint8_t b);

// Main PPU class - inherits from all component classes
class PPU2C02 : public PPU2C02_Memory,
    public PPU2C02_Registers,
    public PPU2C02_Background,
    public PPU2C02_Sprites {
private:
    // Timing state
    int16_t _scanline = 0;
    int16_t _cycle = 0;
    bool _oddFrame = false;
    bool _frameComplete = false;

    // Interrupts
    bool _nmiRequested = false;
    bool _scanlineTrigger = false;

    // Callbacks
    PixelCallback _pixelCallback = nullptr;
    DiagnosticLogCallback _diagnosticCallback = nullptr;

    // Bus connection
    NESBus* _bus = nullptr;

public:
    PPU2C02();
    ~PPU2C02();

    // Override cartridge setter to propagate to memory component
    void SetCartridge(CartridgeInterfaceAPI* cart);

    // Core functions
    void Reset(bool coldstart);
    void Clock();

    // CPU interface (forwarded from PPU2C02_Registers)
    uint8_t CpuRead(uint16_t addr, bool rdOnly = false);
    void CpuWrite(uint16_t addr, uint8_t data);

protected:
public:
    // PPU bus interface (implements pure virtual from base classes)
    uint8_t PpuRead(uint16_t addr, bool rdOnly = false) override;
    void PpuWrite(uint16_t addr, uint8_t data) override;

public:

    // Frame status
    bool IsFrameComplete() const { return _frameComplete; }
    void SetFrameComplete(bool value) { _frameComplete = value; }

    // NMI/Scanline triggers
    bool GetNmiRequested() const { return _nmiRequested; }
    void ClearNmiRequested() { _nmiRequested = false; }
    bool GetScanlineTrigger() const { return _scanlineTrigger; }
    void ClearScanlineTrigger() { _scanlineTrigger = false; }

    // Callbacks
    void SetPixelCallback(PixelCallback callback) { _pixelCallback = callback; }
    void SetDiagnosticCallback(DiagnosticLogCallback callback) { _diagnosticCallback = callback; }

    // Debug helpers
    void GetPatternTable(uint8_t table, uint8_t palette, uint8_t* buffer);
    void GetNameTable(uint8_t index, uint8_t* buffer);

    // Bus connection
    void ConnectBus(NESBus* bus) { _bus = bus; }

    // Public OAM access (for external tools and DLL exports)
    using PPU2C02_Sprites::GetOAM;
    using PPU2C02_Sprites::GetOAMMutable;
    // Public facing OAM functions required for PPU2C02_Registers
    uint8_t ReadOAM() const override { return PPU2C02_Sprites::ReadOAM(); };
    void WriteOAM(uint8_t data) override { PPU2C02_Sprites::WriteOAM(data); };
    void SetOAMAddress(uint8_t addr) override { PPU2C02_Sprites::SetOAMAddress(addr); };

    // Direct OAM access for legacy compatibility
    OAMEntry& GetOAMArray() { return GetOAMMutable(); }


    // Expose GetColorFromPalette for DLL exports
    using PPU2C02_Memory::GetColorFromPalette;
    using PPU2C02_Memory::GetPaletteRam;
    using PPU2C02_Memory::GetSystemPalette;

    // Expose register accessors for DLL exports
    using PPU2C02_Registers::GetControl;
    using PPU2C02_Registers::GetMask;
    using PPU2C02_Registers::GetStatus;

    // Timing accessors for DLL exports
    int16_t GetScanline() const { return _scanline; }
    int16_t GetCycle() const { return _cycle; }

private:
    void PerformBackgroundFetch(int16_t cycle);
    void ProcessCycle(int16_t scanline, int16_t cycle);
private:
    void RenderPixel();
    void Log(const char* msg);
};