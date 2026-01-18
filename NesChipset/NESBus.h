#pragma once

#include <cstdint>
#include <cstring>
#include <mutex>
#include <vector>

// Diagnostics
#include "Diagnostics/DiagnosticHelpers.h"

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif


// Forward declarations - we'll use pointers to avoid linking issues
class Cartridge;
class CartridgeInterfaceAPI;
class PPU2C02;
class CPU6502;
class APU2A03;
class FMODAudioSystem;

// Callback types
typedef double (*AudioSampleCallback)(int64_t sampleIndex, double time);

class NESBus {
public:
    NESBus();
    ~NESBus();

    // Lifecycle
    void Reset(bool poweron);
    bool Clock();  // Returns true when audio sample ready

    // Component connections
    void ConnectCartridge(Cartridge* cart);
    void ConnectPPU(PPU2C02* ppu);
    void ConnectCPU(CPU6502* cpu);
    void ConnectAPU(APU2A03* apu);
    void ConnectAudio(FMODAudioSystem* fmod);

    // CPU Bus interface (called by CPU)
    uint8_t CpuRead(uint16_t addr, bool isReadOnly = false);
    void CpuWrite(uint16_t addr, uint8_t data);

    // Controller interface
    void SetController(uint8_t index, uint8_t state);
    uint8_t GetController(uint8_t index) const;

    // Audio interface
    void SetSampleFrequency(uint32_t sampleRate);
    double GetAudioSample() const { return _audioSample; }
    //int GetAudioBufferLevel() const;
    //bool GetAudioSample(double& sample);  // Pop from ring buffer

    // System state
    uint64_t GetSystemClockCount() const { return _systemClockCounter; }
    bool IsAudioSampleReady() const { return _audioSampleReady; }

    void Tick();
private:
    // Constants
    static constexpr int CPU_RAM_SIZE = 2048;
    static constexpr uint16_t CPU_RAM_MIRROR_MASK = 0x07FF;
    static constexpr uint16_t PPU_REG_MIRROR_MASK = 0x0007;
    static constexpr uint32_t AUDIO_SAMPLE_RATE = 44100;
    static constexpr double NES_MASTER_CLOCK = 5369318.0;
    static constexpr uint32_t AUDIO_RINGBUFFER_SIZE = 8191;  // Power of 2 minus 1

    // Components (pointers to avoid circular dependencies)
    CartridgeInterfaceAPI* _cart;
    PPU2C02* _ppu;
    CPU6502* _cpu;
    APU2A03* _apu;
    FMODAudioSystem* _audio;

    // Memory
    uint8_t _cpuRam[CPU_RAM_SIZE];

    // Controllers
    uint8_t _controllerState[2];
    uint8_t _controllerLatch[2];

    // DMA State
    uint8_t _dmaPage;
    uint8_t _dmaAddr;
    uint8_t _dmaData;
    bool _dmaDummy;
    bool _dmaTransfer;

    // Audio timing
    bool _audioSampleReady;
    double _audioSample;
    double _audioTime;
    double _audioTimePerNESClock;
    double _audioTimePerSystemSample;

    // Audio ring buffer
    //double _nesAudioBuffer[8192];
    //int _audioBufferWrite;
    //int _audioBufferRead;
    double _lastValidSample;
    int64_t _bufferUnderrunCount;

    // System state
    uint64_t _systemClockCounter;

    // Helper functions
    void ProcessDMA();
    bool ProcessAudio();

private:
    bool _loggingEnabled = false;
    static void __stdcall DummyLogger(const char* message) {}
    DiagnosticLogCallback _diagnosticCallback = &DummyLogger;
    bool LoggingEnabled() const { return _loggingEnabled; }
public:
    void EnableLogging(bool enable) { _loggingEnabled = enable; };
    void SetDiagnosticLogCallback(DiagnosticLogCallback callback);
    void Log(const char* msg) const;

public: // Audio methods
    void GenerateAudioFrame();
    int GetAudioSamples(float* buffer, int maxSamples);
    void ResetAudioBuffer();
    bool GetAudioSample(double& sample);
    int GetAudioBufferLevel() const;
private: // Audio buffer variables
    std::vector<float> _nesAudioBuffer;
    mutable std::mutex _audioMutex;
    size_t _audioBufferWrite;  // Write position
    size_t _audioBufferRead;   // Read position

    // Add this constant
    static const size_t AUDIO_BUFFER_CAPACITY = 8192;  // Or whatever size you want
};
