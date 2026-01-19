#include "NESBus.h"
#include <algorithm>
#include <cmath>
#include <iostream>

#include "CartridgeApi/MapperInterfaceAPI.h"
#include "CartridgeApi/CartridgeInterfaceAPI.h"
#include "CPU6502/CPU6502.h"
#include "APU2A03.h"
#include "PPU2C02.h"
#include "FMODAudio/FMODAudioSystem.h"




void NESBus::MeasureAudioLatency() {
    if (!_apu || !_audioSystem) {
        Log("Cannot measure latency - APU or audio system not available");
        return;
    }

    Log("Starting latency measurement...");
    Log("You should hear a beep in a moment. Note the delay from NOW.");

    // Clear the buffer
    {
        std::lock_guard<std::mutex> lock(_audioMutex);
        _audioBufferRead = 0;
        _audioBufferWrite = 0;
    }

    _latencyTestStartClock = _systemClockCounter;
    _latencyTestActive = true;

    // Inject test tone for 0.5 seconds
    _apu->InjectTestTone(true);

    // Run for ~0.5 seconds worth of clocks
    uint64_t clocksFor500ms = static_cast<uint64_t>(PPU_CLOCK_HZ * 0.5);
    uint64_t targetClock = _systemClockCounter + clocksFor500ms;

    while (_systemClockCounter < targetClock) {
        Clock();
    }

    _apu->InjectTestTone(false);

    // Calculate how long it took
    uint64_t elapsedClocks = _systemClockCounter - _latencyTestStartClock;
    double elapsedMs = (elapsedClocks / PPU_CLOCK_HZ) * 1000.0;

    int bufferLevel = GetAudioBufferLevel();
    double bufferMs = (bufferLevel * 1000.0) / _sampleRate;

    int fmodLatency = _audioSystem->GetLatency();
    double totalEstimatedLatency = bufferMs + fmodLatency;

    char msg[512];
    snprintf(msg, sizeof(msg),
        "Latency Test Results:\n"
        "  Generated %.1f ms of audio\n"
        "  Ring buffer: %d samples (%.1f ms)\n"
        "  FMOD latency: %d ms\n"
        "  Total estimated latency: %.1f ms\n"
        "  Listen for the beep and compare to when you started the test",
        elapsedMs, bufferLevel, bufferMs, fmodLatency, totalEstimatedLatency);
    Log(msg);

    _latencyTestActive = false;
}
void NESBus::AdjustAudioBuffer() {
    // Only adjust every ~5 seconds (300 frames at 60fps)
    static int framesSinceLastAdjust = 0;
    if (++framesSinceLastAdjust < 300) return;
    framesSinceLastAdjust = 0;

    int currentLevel = GetAudioBufferLevel();
    float fillPercent = (currentLevel * 100.0f) / AUDIO_BUFFER_CAPACITY;

    // If buffer is consistently too full, we can reduce the target for lower latency
    if (fillPercent > 75.0f && _audioBufferTarget > 2205) {
        _audioBufferTarget -= 441;  // Reduce by 10ms
        char msg[128];
        snprintf(msg, sizeof(msg), "Buffer high (%.1f%%), reducing target to %d samples",
            fillPercent, _audioBufferTarget);
        Log(msg);
    }
    // If buffer is getting low, increase target for stability
    else if (fillPercent < 25.0f && _audioBufferTarget < 8820) {
        _audioBufferTarget += 441;  // Increase by 10ms
        char msg[128];
        snprintf(msg, sizeof(msg), "Buffer low (%.1f%%), increasing target to %d samples",
            fillPercent, _audioBufferTarget);
        Log(msg);
    }
}




#pragma region "Diagnostics"
void NESBus::SetDiagnosticLogCallback(DiagnosticLogCallback callback) {
    _diagnosticCallback = &DummyLogger;
    if (callback) {
        _diagnosticCallback = callback;
    }
}

void NESBus::Log(const char* msg) const {
    if (_diagnosticCallback && _loggingEnabled) {
        _diagnosticCallback(msg);
    }
}
#pragma endregion

NESBus::NESBus()
    : _cart(nullptr), _ppu(nullptr), _apu(nullptr), _cpu(nullptr),
    _dmaPage(0), _dmaAddr(0), _dmaData(0), _dmaDummy(true), _dmaTransfer(false),
    _audioTime(0.0), _systemClockCounter(0), _diagnosticCallback(nullptr),
    _audioBufferWrite(0), _audioBufferRead(0), _sampleRate(44100),
    _audioSampleAccumulator(0.0)  // NEW: track fractional samples
{
    std::memset(_cpuRam, 0, sizeof(_cpuRam));
    std::memset(_controllerState, 0, sizeof(_controllerState));
    std::memset(_controllerLatch, 0, sizeof(_controllerLatch));

    _audioBuffer.resize(AUDIO_BUFFER_CAPACITY, 0.0f);
    _audioTimePerSystemSample = 1.0 / static_cast<double>(_sampleRate);
    _audioTimePerNESClock = 1.0 / PPU_CLOCK_HZ;

    // Calculate how many NES clocks per audio sample
    _nesClocksPerSample = PPU_CLOCK_HZ / static_cast<double>(_sampleRate);

    // Create audio system - DO THIS LAST
    _audioSystem = std::make_unique<FMODAudioSystem>();
    // Connect it to this bus immediately
    if (_audioSystem) {
        _audioSystem->ConnectBus(this);
    }
}

NESBus::~NESBus() {
    if (_cart) delete _cart;
}

void NESBus::ConnectCartridge(Cartridge* cart) {
    if (!_apu) {
        Log("Error: Connecting Cartridge before APU has been connected to the bus.");
        return;
    }
    if (!_cpu) {
        Log("Error: Connecting Cartridge before CPU has been connected to the bus.");
        return;
    }
    if (!_ppu) {
        Log("Error: Connecting Cartridge before PPU has been connected to the bus.");
        return;
    }

    if (_cart) delete _cart;
    _cart = new CartridgeInterfaceAPI(cart);
    if (_cart) _ppu->SetCartridge(_cart);
    if (_cart) _apu->SetCartridge(_cart);
}

void NESBus::ConnectPPU(PPU2C02* ppu) {
    _ppu = ppu;
    if (_ppu) {
        _ppu->ConnectBus(this);
    }
}

void NESBus::ConnectCPU(CPU6502* cpu) {
    _cpu = cpu;
    if (_cpu) {
        _cpu->ConnectBus(this);
    }
}

void NESBus::ConnectAPU(APU2A03* apu) {
    _apu = apu;
    if (_apu) {
        _apu->ConnectBus(this);
    }

    // Connect audio system to APU (bus already connected in constructor)
    if (_audioSystem && _apu) {
        _audioSystem->ConnectAPU(_apu);
    }
}

void NESBus::Reset(bool poweron) {
    // Stop audio if it was playing
    if (_audioSystem && _audioSystem->IsPlaying()) {
        _audioSystem->Stop();
    }

    // Reset cartridge
    if (_cart) _cart->Reset();

    // Reset components
    if (_ppu) _ppu->Reset(poweron);
    if (_apu) _apu->Reset(poweron);
    if (_cpu) _cpu->Reset(poweron);

    if (poweron) {
        std::memset(_cpuRam, 0, sizeof(_cpuRam));
    }

    // Reset DMA
    _dmaPage = _dmaAddr = _dmaData = 0;
    _dmaDummy = true;
    _dmaTransfer = false;

    // Reset audio buffer
    {
        std::lock_guard<std::mutex> lock(_audioMutex);
        std::fill(_audioBuffer.begin(), _audioBuffer.end(), 0.0f);
        _audioBufferRead = 0;
        _audioBufferWrite = 0;
    }
    _audioTime = 0.0;
    _audioSampleAccumulator = 0.0;

    // Reset clock
    _systemClockCounter = 0;

    // Reset controllers
    _controllerState[0] = 0;
    _controllerState[1] = 0;
    _controllerLatch[0] = 0;
    _controllerLatch[1] = 0;
}

uint8_t NESBus::CpuRead(uint16_t addr, bool isReadOnly) {
    uint8_t data = 0;

    if (_cart && _cart->CpuRead(addr, &data)) {
        return data;
    }

    if (addr <= 0x1FFF) {
        return _cpuRam[addr & CPU_RAM_MIRROR_MASK];
    }

    if (addr >= 0x2000 && addr <= 0x3FFF) {
        if (_ppu) {
            return _ppu->CpuRead(addr & PPU_REG_MIRROR_MASK, isReadOnly);
        }
        return 0;
    }

    if (addr == 0x4015) {
        if (_apu) {
            return _apu->CpuRead(addr);
        }
        return 0;
    }

    if (addr >= 0x4016 && addr <= 0x4017) {
        uint8_t controllerIndex = addr & 1;
        data = (_controllerState[controllerIndex] & 0x80) ? 1 : 0;
        _controllerState[controllerIndex] <<= 1;
        return data;
    }

    return 0;
}

void NESBus::CpuWrite(uint16_t addr, uint8_t data) {
    if (_cart && _cart->CpuWrite(addr, data)) {
        return;
    }

    if (addr <= 0x1FFF) {
        _cpuRam[addr & CPU_RAM_MIRROR_MASK] = data;
        return;
    }

    if (addr >= 0x2000 && addr <= 0x3FFF) {
        if (_ppu) {
            _ppu->CpuWrite(addr & PPU_REG_MIRROR_MASK, data);
        }
        return;
    }

    if (addr >= 0x4000 && addr <= 0x4013) {
        if (_apu) {
            _apu->CpuWrite(addr, data);
        }
        return;
    }

    if (addr == 0x4014) {
        _dmaPage = data;
        _dmaAddr = 0;
        _dmaTransfer = true;
        _dmaDummy = true;
        return;
    }

    if (addr == 0x4015 || addr == 0x4017) {
        if (_apu) {
            _apu->CpuWrite(addr, data);
        }
        return;
    }

    if (addr >= 0x4016 && addr <= 0x4017) {
        uint8_t controllerIndex = addr & 1;
        _controllerState[controllerIndex] = _controllerLatch[controllerIndex];
        return;
    }
}

void NESBus::ProcessDMA() {
    if (_dmaDummy) {
        if ((_systemClockCounter % 2) == 1) {
            _dmaDummy = false;
        }
        return;
    }

    if ((_systemClockCounter % 2) == 0) {
        uint16_t addr = (static_cast<uint16_t>(_dmaPage) << 8) | _dmaAddr;
        _dmaData = CpuRead(addr);
    }
    else {
        if (_ppu) {
            uint8_t index = _dmaAddr / 4;
            if (index < 64) {
                _ppu->OAM[index].SetByteAt(_dmaAddr, _dmaData);
            }
        }

        _dmaAddr = (_dmaAddr + 1) & 0xFF;

        if (_dmaAddr == 0) {
            _dmaTransfer = false;
            _dmaDummy = true;
        }
    }
}

void NESBus::ProcessAudio() {
    // Accumulate fractional samples
    _audioSampleAccumulator += 1.0;

    // Generate a sample when we've accumulated enough NES clocks
    while (_audioSampleAccumulator >= _nesClocksPerSample) {
        _audioSampleAccumulator -= _nesClocksPerSample;

        // Get sample from APU
        double sample = _apu->GetOutputSample();

        // Clamp and validate
        sample = std::max(-1.0, std::min(1.0, sample));
        if (std::isnan(sample) || std::isinf(sample)) {
            sample = 0.0;
        }

        // Add to ring buffer (non-blocking, drop if full)
        std::lock_guard<std::mutex> lock(_audioMutex);
        size_t nextWrite = (_audioBufferWrite + 1) % AUDIO_BUFFER_CAPACITY;
        if (nextWrite != _audioBufferRead) {
            _audioBuffer[_audioBufferWrite] = static_cast<float>(sample);
            _audioBufferWrite = nextWrite;
        }
    }
}

bool NESBus::GetAudioSample(float& sample) {
    std::lock_guard<std::mutex> lock(_audioMutex);

    if (_audioBufferRead == _audioBufferWrite) {
        return false;  // Buffer empty
    }

    sample = _audioBuffer[_audioBufferRead];
    _audioBufferRead = (_audioBufferRead + 1) % AUDIO_BUFFER_CAPACITY;
    return true;
}

int NESBus::GetAudioBufferLevel() const {
    std::lock_guard<std::mutex> lock(_audioMutex);

    if (_audioBufferWrite >= _audioBufferRead) {
        return static_cast<int>(_audioBufferWrite - _audioBufferRead);
    }
    else {
        return static_cast<int>(AUDIO_BUFFER_CAPACITY - (_audioBufferRead - _audioBufferWrite));
    }
}

void NESBus::PreFillAudioBuffer(int numSamples) {
    // REMOVED: Don't pre-fill with silence
    // Instead, just start with an empty buffer and let it fill naturally

    std::lock_guard<std::mutex> lock(_audioMutex);

    // Reset buffer pointers
    _audioBufferWrite = 0;
    _audioBufferRead = 0;

    Log("Audio buffer ready - will fill during emulation");
}

bool NESBus::Clock() {
    // CPU runs at 1/3 PPU speed
    if ((_systemClockCounter % 3) == 0) {
        if (_dmaTransfer) {
            ProcessDMA();
        }
        else {
            if (_cpu) {
                _cpu->Clock();
            }
        }
    }

    if (_ppu) {
        _ppu->Clock();
    }

    if (_apu) {
        _apu->Clock();
    }

    // Process audio every clock
    ProcessAudio();

    // Handle NMI from PPU
    if (_ppu && _ppu->GetNmiRequested()) {
        _ppu->ClearNmiRequested();
        if (_cpu) {
            _cpu->NMI();
        }
    }

    // Handle IRQ from cartridge mapper
    if (_cart) {
        MapperInterfaceAPI mapper = _cart->GetMapper();
        if (mapper.IsIrqActive()) {
            mapper.ClearIrq();
            if (_cpu) {
                _cpu->IRQ();
            }
        }
    }

    // Handle IRQ from APU
    if (_apu && _apu->IsIRQActive()) {
        if (_cpu) {
            _cpu->IRQ();
        }
    }

    _systemClockCounter++;
    return true;
}

void NESBus::SetController(uint8_t index, uint8_t state) {
    if (index <= 1) {
        _controllerLatch[index] = state;
    }
}

uint8_t NESBus::GetController(uint8_t index) const {
    if (index <= 1) {
        return _controllerLatch[index];
    }
    return 0;
}

void NESBus::Tick() {
    _ppu->SetFrameComplete(false);
    while (!_ppu->IsFrameComplete()) {
        Clock();
    }
    _ppu->SetFrameComplete(true);

    // Start audio when ready (only runs once)
    if (!_audioSystem->IsPlaying() && _audioSystem) {
        int bufferLevel = GetAudioBufferLevel();
        if (bufferLevel >= _audioBufferTarget) {
            _audioSystem->Start();

            // Optional: Log only on start
            char msg[128];
            snprintf(msg, sizeof(msg),
                "Audio started with %d samples (%.1f ms)",
                bufferLevel, (bufferLevel * 1000.0f) / _sampleRate);
            Log(msg);
        }
    }
    else { GetAudioBufferLevel(); }

    // Update audio system
    if (_audioSystem) {
        _audioSystem->Update();
    }
}

void NESBus::DisplayAudioStatus() {
    
    int bufferLevel = GetAudioBufferLevel();
    float bufferMs = (bufferLevel * 1000.0F) / 44100.0F;
    float bufferPercent = (bufferLevel * 100.0F) / 32768.0F;
    
    // Display something Like
    // "Audio: 4532 samples (102.7ms) [13.8%] Good"
    printf("Audio: %d samples (%.1fms) [%.1f%%] %s\n",
           bufferLevel, bufferMs, bufferPercent,
           (bufferPercent > 20.0f && bufferPercent < 80.0f) ? "Good" : "Poor");
}

// Exported Bus functions
DLLEXPORT NESBus* CreateNESBus() {
    return new NESBus();
}

DLLEXPORT void DestroyNESBus(NESBus* bus) {
    delete bus;
}

DLLEXPORT void Bus_Reset(NESBus* bus, bool poweron) {
    if (bus) bus->Reset(poweron);
}

DLLEXPORT void Bus_Tick(NESBus* bus) {
    if (bus) bus->Tick();
}

DLLEXPORT bool Bus_Clock(NESBus* bus) {
    if (bus) return bus->Clock();
    return false;
}

DLLEXPORT void Bus_ConnectCartridge(NESBus* bus, Cartridge* cart) {
    if (bus) bus->ConnectCartridge(cart);
}

DLLEXPORT void Bus_ConnectPPU(NESBus* bus, PPU2C02* ppu) {
    if (bus) bus->ConnectPPU(ppu);
}

DLLEXPORT void Bus_ConnectCPU(NESBus* bus, CPU6502* cpu) {
    if (bus) bus->ConnectCPU(cpu);
}

DLLEXPORT void Bus_ConnectAPU(NESBus* bus, APU2A03* apu) {
    if (bus) bus->ConnectAPU(apu);
}

DLLEXPORT uint8_t Bus_CpuRead(NESBus* bus, uint16_t addr, bool isReadOnly) {
    if (bus) return bus->CpuRead(addr, isReadOnly);
    return 0;
}

DLLEXPORT void Bus_CpuWrite(NESBus* bus, uint16_t addr, uint8_t data) {
    if (bus) bus->CpuWrite(addr, data);
}

DLLEXPORT void Bus_SetController(NESBus* bus, uint8_t index, uint8_t state) {
    if (bus) bus->SetController(index, state);
}

DLLEXPORT uint8_t Bus_GetController(NESBus* bus, uint8_t index) {
    if (bus) return bus->GetController(index);
    return 0;
}

DLLEXPORT uint64_t Bus_GetSystemClockCount(NESBus* bus) {
    if (bus) return bus->GetSystemClockCount();
    return 0;
}

DLLEXPORT FMODAudioSystem* Bus_GetAudioSystem(NESBus* bus) {
    if (!bus) {
        std::cerr << "[ERROR] Bus_GetAudioSystem called with null bus" << std::endl;
        return nullptr;
    }

    FMODAudioSystem* audio = bus->GetAudioSystem();
    if (!audio) {
        std::cerr << "[ERROR] Bus has null audio system" << std::endl;
    }

    return audio;
}

DLLEXPORT void Bus_PreFillAudioBuffer(NESBus* bus, int numSamples) {
    if (bus) {
        bus->PreFillAudioBuffer(numSamples);
    }
}

DLLEXPORT void BusEnableDiagnosticLogger(NESBus* bus, bool enable) {
    if (bus) bus->EnableLogging(enable);
}

DLLEXPORT void BusSetDiagnosticLogCallback(NESBus* bus, DiagnosticLogCallback callback) {
    if (bus && callback) {
        bus->SetDiagnosticLogCallback(callback);
        bus->Log("Info: Diagnostic Log Callback attached successfully.");
    }
    else if (bus == nullptr) {
        if (callback) callback("Error: Bus instance is nullptr.");
    }
}

DLLEXPORT void Bus_MeasureAudioLatency(NESBus* bus) {
    if (bus) bus->MeasureAudioLatency();
}