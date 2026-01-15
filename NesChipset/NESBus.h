#pragma once

#include <cstdint>
#include <cstring>

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif


// Forward declarations - we'll use pointers to avoid linking issues
class Cartridge;
class CartridgeInterface;
class PPU2C02;
class CPU6502;
class APU2A03;

// Callback types
typedef void (*DiagnosticCallback)(const char* msg);
typedef double (*AudioSampleCallback)(int64_t sampleIndex, double time);

#pragma region "CPU API CALLBACKS"
// CPU .Net API
typedef void(__stdcall* ApiCpuClock)();
typedef void(__stdcall* ApiCpuReset)();
typedef void(__stdcall* ApiCpuTriggerNmi)();
typedef void(__stdcall* ApiCpuTriggerIrq)();
struct CpuApiCallbacks {
    ApiCpuClock Clock;
    ApiCpuReset Reset;
    ApiCpuTriggerNmi TriggerNmi;
    ApiCpuTriggerIrq TriggerIrq;
};
// APU .Net API
//typedef void(__stdcall* ApiApuClock)();
//typedef void(__stdcall* ApiApuReset)();
//typedef uint8_t(__stdcall* ApiApuCpuRead)(uint16_t);
//typedef void(__stdcall* ApiApuCpuWrite)(uint16_t, uint8_t);
//typedef double(__stdcall* ApiApuGetOutputSample)();
//struct ApuApiCallbacks {
//    ApiApuClock Clock;
//    ApiApuReset Reset;
//    ApiApuCpuRead CpuRead;
//    ApiApuCpuWrite CpuWrite;
//    ApiApuGetOutputSample GetSoundSample;
//};
#pragma endregion

class NESBus {
private: // Temporary API CPU and APU .Net callbacks
    CpuApiCallbacks CPUApi = { nullptr,nullptr,nullptr,nullptr };
public:
    void SetCPUApi(CpuApiCallbacks api) { CPUApi = api; };
public:
    NESBus();
    ~NESBus();

    // Lifecycle
    void Reset();
    bool Clock();  // Returns true when audio sample ready

    // Component connections
    void ConnectCartridge(Cartridge* cart);
    void ConnectPPU(PPU2C02* ppu);
    void ConnectCPU(CPU6502* cpu);
    void ConnectAPU(APU2A03* apu);

    // CPU Bus interface (called by CPU)
    uint8_t CpuRead(uint16_t addr, bool isReadOnly = false);
    void CpuWrite(uint16_t addr, uint8_t data);

    // Controller interface
    void SetController(uint8_t index, uint8_t state);
    uint8_t GetController(uint8_t index) const;

    // Audio interface
    void SetSampleFrequency(uint32_t sampleRate);
    double GetAudioSample() const { return _audioSample; }
    int GetAudioBufferLevel() const;
    bool GetAudioSample(double& sample);  // Pop from ring buffer

    // System state
    uint64_t GetSystemClockCount() const { return _systemClockCounter; }
    bool IsAudioSampleReady() const { return _audioSampleReady; }

    // Callbacks
    void SetDiagnosticCallback(DiagnosticCallback callback) { _diagnosticCallback = callback; }

private:
    // Constants
    static constexpr int CPU_RAM_SIZE = 2048;
    static constexpr uint16_t CPU_RAM_MIRROR_MASK = 0x07FF;
    static constexpr uint16_t PPU_REG_MIRROR_MASK = 0x0007;
    static constexpr uint32_t AUDIO_SAMPLE_RATE = 44100;
    static constexpr double NES_MASTER_CLOCK = 5369318.0;
    static constexpr uint32_t AUDIO_RINGBUFFER_SIZE = 8191;  // Power of 2 minus 1

    // Components (pointers to avoid circular dependencies)
    CartridgeInterface* _cart;
    PPU2C02* _ppu;
    //CPU6502* _cpu;
    APU2A03* _apu;

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
    double _audioBuffer[AUDIO_RINGBUFFER_SIZE + 1];
    int _audioBufferWrite;
    int _audioBufferRead;
    double _lastValidSample;
    int64_t _bufferUnderrunCount;

    // System state
    uint64_t _systemClockCounter;

    // Callbacks
    DiagnosticCallback _diagnosticCallback;

    // Helper functions
    void ProcessDMA();
    bool ProcessAudio();
    void Log(const char* msg);
};
