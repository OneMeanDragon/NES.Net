#include "FMODAudioSystem.h"
#include "../NESBus.h"
#include "../APU2A03.h"
#include <fmod_errors.h>
#include <iostream>
#include <algorithm>

FMODAudioSystem::FMODAudioSystem()
    : _system(nullptr), _sound(nullptr), _channel(nullptr), _bus(nullptr),
    _sampleRate(44100), _bufferSize(512), _volume(1.0f),
    _initialized(false), _playing(false), _underrunCount(0)
{
}

FMODAudioSystem::~FMODAudioSystem() {
    if (_initialized) {
        Stop();

        if (_sound) {
            _sound->release();
            _sound = nullptr;
        }

        if (_system) {
            _system->close();
            _system->release();
            _system = nullptr;
        }
    }
}

void FMODAudioSystem::LogFMODError(FMOD_RESULT result, const char* function) {
    if (result != FMOD_OK) {
        std::cerr << "[FMOD] Error in " << function << ": "
            << FMOD_ErrorString(result) << std::endl;
    }
}

bool FMODAudioSystem::Initialize(int sampleRate, int bufferSize) {
    if (_initialized) {
        std::cerr << "[FMOD] Already initialized" << std::endl;
        return false;
    }

    _sampleRate = sampleRate;
    _bufferSize = bufferSize;

    FMOD_RESULT result;

    result = FMOD::System_Create(&_system);
    if (result != FMOD_OK) {
        LogFMODError(result, "System_Create");
        return false;
    }

    unsigned int version;
    result = _system->getVersion(&version);
    if (result != FMOD_OK) {
        LogFMODError(result, "getVersion");
        return false;
    }

    if (version < FMOD_VERSION) {
        std::cerr << "[FMOD] Version mismatch" << std::endl;
        return false;
    }

    // Use 2 buffers for lower latency
    result = _system->setDSPBufferSize(_bufferSize, 2);
    if (result != FMOD_OK) {
        LogFMODError(result, "setDSPBufferSize");
    }

    // Set software format to match our sample rate
    result = _system->setSoftwareFormat(_sampleRate, FMOD_SPEAKERMODE_MONO, 0);
    if (result != FMOD_OK) {
        LogFMODError(result, "setSoftwareFormat");
    }

    result = _system->init(32, FMOD_INIT_NORMAL, nullptr);
    if (result != FMOD_OK) {
        LogFMODError(result, "init");
        _system->release();
        _system = nullptr;
        return false;
    }

    FMOD_CREATESOUNDEXINFO exinfo = {};
    exinfo.cbsize = sizeof(FMOD_CREATESOUNDEXINFO);

    // CRITICAL FIX: Make the decode buffer match the DSP buffer size
    // This makes FMOD request smaller chunks that match what we can provide
    // Instead of 2 seconds (88,200 samples), use 4x the buffer size
    exinfo.decodebuffersize = _bufferSize * 4;  // 2048 samples at 512 buffer size
    exinfo.length = _bufferSize * 4 * sizeof(float);  // Size in bytes

    exinfo.numchannels = 1;
    exinfo.defaultfrequency = _sampleRate;
    exinfo.format = FMOD_SOUND_FORMAT_PCMFLOAT;
    exinfo.pcmreadcallback = &FMODAudioSystem::PCMReadCallback;
    exinfo.userdata = this;

    result = _system->createSound(
        nullptr,
        FMOD_LOOP_NORMAL | FMOD_OPENUSER | FMOD_CREATESTREAM,
        &exinfo,
        &_sound);

    if (result != FMOD_OK) {
        LogFMODError(result, "createSound");
        _system->close();
        _system->release();
        _system = nullptr;
        return false;
    }

    _initialized = true;

    std::cout << "[FMOD] Initialized: " << _sampleRate << " Hz, "
        << _bufferSize << " samples/buffer, 2 buffers" << std::endl;
    std::cout << "[FMOD] Decode buffer: " << exinfo.decodebuffersize
        << " samples (~" << (exinfo.decodebuffersize * 1000 / _sampleRate)
        << " ms)" << std::endl;

    return true;
}

void FMODAudioSystem::Start() {
    if (!_initialized) {
        std::cerr << "[FMOD] Cannot start - not initialized" << std::endl;
        return;
    }

    if (_playing) {
        // Already playing, just update
        return;
    }

    FMOD_RESULT result = _system->playSound(_sound, nullptr, false, &_channel);
    if (result != FMOD_OK) {
        LogFMODError(result, "playSound");
        return;
    }

    if (_channel) {
        _channel->setVolume(_volume);
        _channel->setPriority(0);
    }

    _playing = true;
    std::cout << "[FMOD] Playback started" << std::endl;
}

void FMODAudioSystem::Stop() {
    if (!_playing) {
        return;
    }

    if (_channel) {
        _channel->stop();
        _channel = nullptr;
    }

    _playing = false;
    std::cout << "[FMOD] Playback stopped" << std::endl;
}

void FMODAudioSystem::Pause(bool pause) {
    if (_channel) {
        _channel->setPaused(pause);
    }
}

void FMODAudioSystem::Update() {
    if (_system) {
        _system->update();
    }
}

void FMODAudioSystem::SetVolume(float volume) {
    _volume = std::max(0.0f, std::min(1.0f, volume));
    if (_channel) {
        _channel->setVolume(_volume);
    }
}

float FMODAudioSystem::GetVolume() const {
    return _volume;
}

bool FMODAudioSystem::IsPlaying() const {
    if (!_channel) {
        return false;
    }

    bool playing = false;
    _channel->isPlaying(&playing);
    return playing;
}

int FMODAudioSystem::GetLatency() const {
    return (_bufferSize * 1000) / _sampleRate;
}

FMODAudioSystem::AudioStats FMODAudioSystem::GetStats() const {
    AudioStats stats = {};

    if (_bus) {
        int bufferLevel = _bus->GetAudioBufferLevel();
        int maxBuffer = 2048;
        stats.bufferFillPercent = (bufferLevel * 100) / maxBuffer;
    }

    stats.underrunCount = _underrunCount;
    stats.latencyMs = GetLatency();

    if (_system) {
        FMOD_CPU_USAGE usage;
        _system->getCPUUsage(&usage);
        stats.cpuUsage = usage.dsp + usage.stream + usage.geometry + usage.update;
    }

    return stats;
}

FMOD_RESULT F_CALL FMODAudioSystem::PCMReadCallback(
    FMOD_SOUND* sound,
    void* data,
    unsigned int datalen)
{
    void* userdata = nullptr;
    FMOD::Sound* soundObj = (FMOD::Sound*)sound;
    soundObj->getUserData(&userdata);

    if (!userdata) {
        memset(data, 0, datalen);
        return FMOD_OK;
    }

    FMODAudioSystem* audioSystem = static_cast<FMODAudioSystem*>(userdata);
    return audioSystem->FillAudioBuffer(data, datalen);
}

FMOD_RESULT FMODAudioSystem::FillAudioBuffer(void* data, unsigned int datalen) {
    if (!_bus) {
        memset(data, 0, datalen);
        return FMOD_OK;
    }

    float* samples = static_cast<float*>(data);
    int sampleCount = datalen / sizeof(float);

    int samplesRead = 0;
    float lastSample = 0.0f;

    // Try to read all requested samples
    for (int i = 0; i < sampleCount; i++) {
        float sample;
        if (_bus->GetAudioSample(sample)) {
            samples[i] = sample;
            lastSample = sample;
            samplesRead++;
        }
        else {
            // Buffer underrun - hold last sample
            samples[i] = lastSample;
        }
    }

    // Count underruns (only when we got less than 50% of requested samples)
    if (samplesRead < sampleCount / 2) {
        _underrunCount++;

        // Log severe underruns
        static int logCounter = 0;
        if (++logCounter % 100 == 0) {
            std::cout << "[FMOD] Severe underrun: requested " << sampleCount
                << ", got " << samplesRead << " (buffer may be too small)" << std::endl;
        }
    }

    return FMOD_OK;
}

FMODAudioSystem::AudioHealth FMODAudioSystem::GetAudioHealth() const {
    AudioHealth health = {};

    if (_bus) {
        health.bufferLevel = _bus->GetAudioBufferLevel();
        health.bufferPercent = (health.bufferLevel * 100.0f) / 32768.0f;
    }

    health.underruns = _underrunCount;
    health.avgLatencyMs = GetLatency();

    // Consider stable if buffer is between 25-75% full and underruns are minimal
    health.stable = (health.bufferPercent > 25.0f && health.bufferPercent < 75.0f);

    return health;
}

// Exports
DLLEXPORT FMODAudioSystem* FMODAudio_Create() {
    return new FMODAudioSystem();
}

DLLEXPORT void FMODAudio_Destroy(FMODAudioSystem* audio) {
    delete audio;
}

DLLEXPORT bool FMODAudio_Initialize(FMODAudioSystem* audio, int sampleRate, int bufferSize) {
    if (audio) return audio->Initialize(sampleRate, bufferSize);
    return false;
}

DLLEXPORT void FMODAudio_Start(FMODAudioSystem* audio) {
    if (audio) audio->Start();
}

DLLEXPORT void FMODAudio_Stop(FMODAudioSystem* audio) {
    if (audio) audio->Stop();
}

DLLEXPORT void FMODAudio_Pause(FMODAudioSystem* audio, bool pause) {
    if (audio) audio->Pause(pause);
}

DLLEXPORT void FMODAudio_Update(FMODAudioSystem* audio) {
    if (audio) audio->Update();
}

DLLEXPORT void FMODAudio_SetVolume(FMODAudioSystem* audio, float volume) {
    if (audio) audio->SetVolume(volume);
}

DLLEXPORT float FMODAudio_GetVolume(FMODAudioSystem* audio) {
    if (audio) return audio->GetVolume();
    return 0.0f;
}

DLLEXPORT bool FMODAudio_IsPlaying(FMODAudioSystem* audio) {
    if (audio) return audio->IsPlaying();
    return false;
}

DLLEXPORT int FMODAudio_GetLatency(FMODAudioSystem* audio) {
    if (audio) return audio->GetLatency();
    return 0;
}

DLLEXPORT bool FMODAudio_IsStable(FMODAudioSystem* audio) {
    if (audio) {
        auto health = audio->GetAudioHealth();
        return health.stable;
    }
    return false;
}