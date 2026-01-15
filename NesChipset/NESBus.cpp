#include "NESBus.h"

#include <algorithm>
#include <cmath>
#include <core/Interfaces/CartridgeInterface.h>

#include "APU2A03.h"
#include "BusInterfaces.h"

NESBus::NESBus()
    : _cart(nullptr), _ppu(nullptr), _apu(nullptr),//_cpu(nullptr),
    _dmaPage(0), _dmaAddr(0), _dmaData(0), _dmaDummy(true), _dmaTransfer(false),
    _audioSampleReady(false), _audioSample(0.0), _audioTime(0.0),
    _audioBufferWrite(0), _audioBufferRead(0), _lastValidSample(0.0),
    _bufferUnderrunCount(0), _systemClockCounter(0), _diagnosticCallback(nullptr)
{
    std::memset(_cpuRam, 0, sizeof(_cpuRam));
    std::memset(_controllerState, 0, sizeof(_controllerState));
    std::memset(_controllerLatch, 0, sizeof(_controllerLatch));
    std::memset(_audioBuffer, 0, sizeof(_audioBuffer));

    SetSampleFrequency(AUDIO_SAMPLE_RATE);
}

NESBus::~NESBus() {
    if (_cart) delete _cart;
}

void NESBus::Log(const char* msg) {
    if (_diagnosticCallback) {
        _diagnosticCallback(msg);
    }
}

void NESBus::ConnectCartridge(Cartridge* cart) {
    if (!_apu) {
        Log("Warning: Connecting Cartridge before APU. APU must be connected first.");
        return;
	}
    if (_cart) delete _cart;
    _cart = new CartridgeInterface(cart);
    if (_cart) _apu->SetCartridge(_cart);
}

void NESBus::ConnectPPU(PPU2C02* ppu) {
    _ppu = ppu;
}

void NESBus::ConnectCPU(CPU6502* cpu) {
    //_cpu = cpu;
}

void NESBus::ConnectAPU(APU2A03* apu) {
    _apu = apu;
}

void NESBus::SetSampleFrequency(uint32_t sampleRate) {
    _audioTimePerSystemSample = 1.0 / sampleRate;
    _audioTimePerNESClock = 1.0 / NES_MASTER_CLOCK;
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

void NESBus::Reset() {
    // Reset cartridge
    if (_cart) {
        _cart->Reset();
    }

    // Reset components
    //if (_cpu) {
    //    ResetCPU(_cpu);
    //}
    if (CPUApi.Reset) {
        CPUApi.Reset();
    }

    if (_ppu) {
        PPU_Reset(_ppu);
    }
    if (_apu) {
        ResetAPU(_apu);
    }

    // Clear RAM
    std::memset(_cpuRam, 0, sizeof(_cpuRam));

    // Reset DMA
    _dmaPage = 0;
    _dmaAddr = 0;
    _dmaData = 0;
    _dmaDummy = true;
    _dmaTransfer = false;

    // Reset audio
    _audioSample = 0.0;
    _audioTime = 0.0;
    _audioSampleReady = false;

    // Clear audio buffer
    std::memset(_audioBuffer, 0, sizeof(_audioBuffer));
    _audioBufferWrite = 0;
    _audioBufferRead = 0;
    _lastValidSample = 0.0;
    _bufferUnderrunCount = 0;

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

    // Try cartridge first
    if (_cart && _cart->CpuRead(addr, data)) {
        return data;
    }

    // CPU RAM ($0000-$1FFF, mirrored every 2KB)
    if (addr <= 0x1FFF) {
        return _cpuRam[addr & CPU_RAM_MIRROR_MASK];
    }

    // PPU Registers ($2000-$3FFF, mirrored every 8 bytes)
    if (addr >= 0x2000 && addr <= 0x3FFF) {
        if (_ppu) {
            return PPU_CpuRead(_ppu, addr & PPU_REG_MIRROR_MASK, isReadOnly);
        }
        return 0;
    }

    // APU Status ($4015)
    if (addr == 0x4015) {
        if (_apu) {
            return APU_CpuRead(_apu, addr);
        }
        return 0;
    }

    // Controller reads ($4016-$4017)
    if (addr >= 0x4016 && addr <= 0x4017) {
        uint8_t controllerIndex = addr & 1;
        data = (_controllerState[controllerIndex] & 0x80) ? 1 : 0;
        _controllerState[controllerIndex] <<= 1;
        return data;
    }

    // Open bus
    return 0;
}

void NESBus::CpuWrite(uint16_t addr, uint8_t data) {
    // Try cartridge first
    if (_cart && _cart->CpuWrite(addr, data)) {
        return;
    }

    // CPU RAM ($0000-$1FFF, mirrored every 2KB)
    if (addr <= 0x1FFF) {
        _cpuRam[addr & CPU_RAM_MIRROR_MASK] = data;
        return;
    }

    // PPU Registers ($2000-$3FFF, mirrored every 8 bytes)
    if (addr >= 0x2000 && addr <= 0x3FFF) {
        if (_ppu) {
            PPU_CpuWrite(_ppu, addr & PPU_REG_MIRROR_MASK, data);
        }
        return;
    }

    // APU and I/O registers ($4000-$4013)
    if (addr >= 0x4000 && addr <= 0x4013) {
        if (_apu) {
            APU_CpuWrite(_apu, addr, data);
        }
        return;
    }

    // OAM DMA ($4014)
    if (addr == 0x4014) {
        _dmaPage = data;
        _dmaAddr = 0;
        _dmaTransfer = true;
        _dmaDummy = true;
        return;
    }

    // APU registers ($4015, $4017)
    if (addr == 0x4015 || addr == 0x4017) {
        if (_apu) {
            APU_CpuWrite(_apu, addr, data);
        }
        return;
    }

    // Controller strobe ($4016-$4017)
    if (addr >= 0x4016 && addr <= 0x4017) {
        uint8_t controllerIndex = addr & 1;
        _controllerState[controllerIndex] = _controllerLatch[controllerIndex];
        return;
    }
}

void NESBus::ProcessDMA() {
    // Wait for even cycle to start
    if (_dmaDummy) {
        if ((_systemClockCounter % 2) == 1) {
            _dmaDummy = false;
        }
        return;
    }

    // Perform DMA transfer
    if ((_systemClockCounter % 2) == 0) {
        // Even cycle: Read from CPU memory
        uint16_t addr = (static_cast<uint16_t>(_dmaPage) << 8) | _dmaAddr;
        _dmaData = CpuRead(addr);
    }
    else {
        // Odd cycle: Write to OAM
        if (_ppu) {
            PPU_SetOAMByte(_ppu, _dmaAddr, _dmaData);
        }

        // Increment address
        _dmaAddr = (_dmaAddr + 1) & 0xFF;

        // Check if DMA complete
        if (_dmaAddr == 0) {
            _dmaTransfer = false;
            _dmaDummy = true;
        }
    }
}

bool NESBus::ProcessAudio() {
    _audioTime += _audioTimePerNESClock;

    if (_audioTime >= _audioTimePerSystemSample) {
        _audioTime -= _audioTimePerSystemSample;

        // Get sample from APU
        if (_apu) {
            _audioSample = APU_GetOutputSample(_apu);
        }
        else {
            _audioSample = 0.0;
        }

        // Clamp sample
        _audioSample = std::max(-1.0, std::min(1.0, _audioSample));

        // Filter out NaN and Infinity
        if (std::isnan(_audioSample) || std::isinf(_audioSample)) {
            _audioSample = 0.0;
        }

        // Add to ring buffer
        int nextWrite = (_audioBufferWrite + 1) & AUDIO_RINGBUFFER_SIZE;
        if (nextWrite != _audioBufferRead) {
            _audioBuffer[_audioBufferWrite] = _audioSample;
            _audioBufferWrite = nextWrite;
        }

        return true;
    }

    return false;
}

int NESBus::GetAudioBufferLevel() const {
    int diff = _audioBufferWrite - _audioBufferRead;
    if (diff < 0) {
        diff += (AUDIO_RINGBUFFER_SIZE + 1);
    }
    return diff;
}

bool NESBus::GetAudioSample(double& sample) {
    int available = _audioBufferWrite - _audioBufferRead;
    if (available < 0) {
        available += (AUDIO_RINGBUFFER_SIZE + 1);
    }

    if (available > 0) {
        sample = _audioBuffer[_audioBufferRead];
        _audioBufferRead = (_audioBufferRead + 1) & AUDIO_RINGBUFFER_SIZE;

        // Clamp to valid range
        sample = std::max(-1.0, std::min(1.0, sample));
        _lastValidSample = sample;
        return true;
    }
    else {
        // Buffer underrun
        _bufferUnderrunCount++;
        sample = _lastValidSample;
        return false;
    }
}

bool NESBus::Clock() {
    // Clock PPU
    if (_ppu) {
        PPU_Clock(_ppu);
    }

    // Clock APU
    if (_apu) {
        ClockAPU(_apu);
    }

    // CPU runs at 1/3 PPU speed
    if ((_systemClockCounter % 3) == 0) {
        if (_dmaTransfer) {
            ProcessDMA();
        }
        else {
            //if (_cpu) {
            //    ClockCPU(_cpu);
            //}
            if (CPUApi.Clock) {
                CPUApi.Clock();
            }
        }
    }

    // Process audio timing
    _audioSampleReady = ProcessAudio();

    // Handle NMI from PPU
    if (_ppu && PPU_GetNmiRequested(_ppu)) {
        PPU_ClearNmiRequested(_ppu);
        //if (_cpu) {
        //    TriggerNMI(_cpu);
        //}
        if (CPUApi.TriggerNmi) {
            CPUApi.TriggerNmi();
        }
    }

    // Handle IRQ from cartridge mapper
    if (_cart) {
        MapperBase* mapper = _cart->GetMapper();
        if (mapper && MapperIsIrqActive(mapper)) {
            MapperClearIrq(mapper);
            //if (_cpu) {
            //    TriggerIRQ(_cpu);
            //}
            if (CPUApi.TriggerIrq) {
                CPUApi.TriggerIrq();
            }
        }
    }

    // Increment system clock
    _systemClockCounter++;

    return _audioSampleReady;
}

// Exported Bus functions
DLLEXPORT NESBus* CreateNESBus() {
    return new NESBus();
}

DLLEXPORT void DestroyNESBus(NESBus* bus) {
    delete bus;
}

DLLEXPORT void Bus_Reset(NESBus* bus) {
    if (bus) bus->Reset();
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

DLLEXPORT void Bus_SetSampleFrequency(NESBus* bus, uint32_t sampleRate) {
    if (bus) bus->SetSampleFrequency(sampleRate);
}

DLLEXPORT double Bus_GetAudioSample(NESBus* bus) {
    if (bus) return bus->GetAudioSample();
    return 0.0;
}

DLLEXPORT int Bus_GetAudioBufferLevel(NESBus* bus) {
    if (bus) return bus->GetAudioBufferLevel();
    return 0;
}

DLLEXPORT bool Bus_PopAudioSample(NESBus* bus, double* sample) {
    if (bus && sample) return bus->GetAudioSample(*sample);
    return false;
}

DLLEXPORT uint64_t Bus_GetSystemClockCount(NESBus* bus) {
    if (bus) return bus->GetSystemClockCount();
    return 0;
}

DLLEXPORT bool Bus_IsAudioSampleReady(NESBus* bus) {
    if (bus) return bus->IsAudioSampleReady();
    return false;
}

DLLEXPORT void Bus_SetDiagnosticCallback(NESBus* bus, DiagnosticCallback callback) {
    if (bus) bus->SetDiagnosticCallback(callback);
}

DLLEXPORT void UpdateCPUApi(NESBus* bus, CpuApiCallbacks api) {
    if (bus) bus->SetCPUApi(api);
}