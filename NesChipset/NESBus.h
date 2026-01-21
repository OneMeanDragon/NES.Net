#pragma once

#include <cstdint>
#include <cstring>
#include <mutex>
#include <vector>
#include <memory>

// Diagnostics
#include "Diagnostics/DiagnosticHelpers.h"

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif

// Forward declarations
class Cartridge;
class CartridgeInterfaceAPI;
class PPU2C02;
class CPU6502;
class APU2A03;
class FMODAudioSystem;

class NESBus {
public:
    uint8_t _openBus = 0;
public:
    NESBus();
    ~NESBus();

    // Lifecycle
    void Reset(bool poweron);
    bool Clock();

    // Component connections
    void ConnectCartridge(Cartridge* cart);
    void ConnectPPU(PPU2C02* ppu);
    void ConnectCPU(CPU6502* cpu);
    void ConnectAPU(APU2A03* apu);

    // CPU Bus interface
    uint8_t CpuRead(uint16_t addr, bool isReadOnly = false);
    void CpuWrite(uint16_t addr, uint8_t data);

    // Controller interface
    void SetController(uint8_t index, uint8_t state);
    uint8_t GetController(uint8_t index) const;

    // System state
    uint64_t GetSystemClockCount() const { return _systemClockCounter; }

    void Tick();

    // Audio system access
    FMODAudioSystem* GetAudioSystem() const { return _audioSystem.get(); }

    // Audio buffer interface for FMODAudioSystem
    bool GetAudioSample(float& sample);
    int GetAudioBufferLevel() const;
    void PreFillAudioBuffer(int numSamples);
public:
    void SetAudioBufferTarget(int samples) { _audioBufferTarget = samples; }
    int GetAudioBufferTarget() const { return _audioBufferTarget; }
private:
    int _audioBufferTarget = 2205;// 2205: 50ms - risky but lowest latency [4410: 100ms instead of 200ms] // was 8820
private:
    // Constants
    static constexpr int CPU_RAM_SIZE = 2048;
    static constexpr uint16_t CPU_RAM_MIRROR_MASK = 0x07FF;
    static constexpr uint16_t PPU_REG_MIRROR_MASK = 0x0007;

    static constexpr double MHZ = 1000000.0;
    static constexpr double NTSC_MASTER_CRYSTAL_MHZ = 21.477272;
    static constexpr double  PAL_MASTER_CRYSTAL_MHZ = 26.601712;

    static constexpr double NTSC_MASTER_CLOCK_HZ = NTSC_MASTER_CRYSTAL_MHZ * MHZ;
    static constexpr double  PAL_MASTER_CLOCK_HZ = PAL_MASTER_CRYSTAL_MHZ * MHZ;

    static constexpr double CPU_CLOCK_HZ = (NTSC_MASTER_CLOCK_HZ / 12); // NTSC master hz frequencys
    static constexpr double PPU_CLOCK_HZ = (NTSC_MASTER_CLOCK_HZ / 4);
    static constexpr double PAL_CPU_CLOCK_HZ = (PAL_MASTER_CLOCK_HZ / 16); // PAL master hz frequencys
    static constexpr double PAL_PPU_CLOCK_HZ = (PAL_MASTER_CLOCK_HZ / 5);

    static constexpr size_t AUDIO_BUFFER_CAPACITY = 32768;  // Larger buffer for stability

    // Components
    CartridgeInterfaceAPI* _cart;
    PPU2C02* _ppu;
    CPU6502* _cpu;
    APU2A03* _apu;
    std::unique_ptr<FMODAudioSystem> _audioSystem;

    // Memory
    uint8_t _cpuRam[CPU_RAM_SIZE];

    // Controllers
    uint8_t _controllerState[2];
    uint8_t _controllerLatch[2];
    void UpdateNESController();

    // DMA State
    uint8_t _dmaPage;
    uint8_t _dmaAddr;
    uint8_t _dmaData;
    bool _dmaDummy;
    bool _dmaTransfer;

    // Audio timing
    double _audioTime;
    double _audioTimePerNESClock;
    double _audioTimePerSystemSample;
    uint32_t _sampleRate;
    double _nesClocksPerSample;        // How many NES clocks per audio sample
    double _audioSampleAccumulator;    // Tracks fractional samples

    // System state
    uint64_t _systemClockCounter;

    // Audio ring buffer
    std::vector<float> _audioBuffer;
    size_t _audioBufferWrite;
    size_t _audioBufferRead;
    mutable std::mutex _audioMutex;

    // Helper functions
    void ProcessDMA();
    void ProcessAudio();

    // Diagnostics
    bool _loggingEnabled = false;
    static void __stdcall DummyLogger(const char* message) {}
    DiagnosticLogCallback _diagnosticCallback = &DummyLogger;
    bool LoggingEnabled() const { return _loggingEnabled; }

public:
    void EnableLogging(bool enable) { _loggingEnabled = enable; }
    void SetDiagnosticLogCallback(DiagnosticLogCallback callback);
    void Log(const char* msg) const;

public:
    void MeasureAudioLatency();
private:
    uint64_t _latencyTestStartClock = 0;
    bool _latencyTestActive = false;
public:
    void AdjustAudioBuffer();  // Call this occasionally to tune buffer size
private:
    int _underrunCounter = 0;
    int _lastBufferAdjustmentFrame = 0;
    void DisplayAudioStatus();
public:
    void Stop();
};

// Exported Bus functions
DLLEXPORT NESBus* CreateNESBus();
DLLEXPORT void DestroyNESBus(NESBus* bus);
DLLEXPORT void Bus_Reset(NESBus* bus, bool poweron);
DLLEXPORT void Bus_Tick(NESBus* bus);
DLLEXPORT bool Bus_Clock(NESBus* bus);

DLLEXPORT void Bus_ConnectCartridge(NESBus* bus, Cartridge* cart);
DLLEXPORT void Bus_ConnectPPU(NESBus* bus, PPU2C02* ppu);
DLLEXPORT void Bus_ConnectCPU(NESBus* bus, CPU6502* cpu);
DLLEXPORT void Bus_ConnectAPU(NESBus* bus, APU2A03* apu);

DLLEXPORT uint8_t Bus_CpuRead(NESBus* bus, uint16_t addr, bool isReadOnly);
DLLEXPORT void Bus_CpuWrite(NESBus* bus, uint16_t addr, uint8_t data);

DLLEXPORT void Bus_SetController(NESBus* bus, uint8_t index, uint8_t state);
DLLEXPORT uint8_t Bus_GetController(NESBus* bus, uint8_t index);

DLLEXPORT uint64_t Bus_GetSystemClockCount(NESBus* bus);

// Audio system exports
DLLEXPORT FMODAudioSystem* Bus_GetAudioSystem(NESBus* bus);
DLLEXPORT void Bus_PreFillAudioBuffer(NESBus* bus, int numSamples);

// Diagnostics
DLLEXPORT void BusEnableDiagnosticLogger(NESBus* bus, bool enable);
DLLEXPORT void BusSetDiagnosticLogCallback(NESBus* bus, DiagnosticLogCallback callback);