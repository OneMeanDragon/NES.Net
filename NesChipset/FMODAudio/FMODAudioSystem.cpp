#include "FMODAudioSystem.h"
#include "../NESBus.h"
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

    // Create FMOD system
    result = FMOD::System_Create(&_system);
    if (result != FMOD_OK) {
        LogFMODError(result, "System_Create");
        return false;
    }

    // Get FMOD version
    unsigned int version;
    result = _system->getVersion(&version);
    if (result != FMOD_OK) {
        LogFMODError(result, "getVersion");
        return false;
    }

    if (version < FMOD_VERSION) {
        std::cerr << "[FMOD] Version mismatch. Header: " << FMOD_VERSION
            << ", DLL: " << version << std::endl;
        return false;
    }

    // Set DSP buffer size for low latency
    result = _system->setDSPBufferSize(_bufferSize, 2); // 2
    if (result != FMOD_OK) {
        LogFMODError(result, "setDSPBufferSize");
        // Non-fatal, continue
    }

    // Initialize FMOD
    result = _system->init(32, FMOD_INIT_NORMAL, nullptr); //32
    if (result != FMOD_OK) {
        LogFMODError(result, "init");
        _system->release();
        _system = nullptr;
        return false;
    }

    // Create sound info for streaming PCM
    FMOD_CREATESOUNDEXINFO exinfo = {};
    exinfo.cbsize = sizeof(FMOD_CREATESOUNDEXINFO);
    exinfo.length = _sampleRate * sizeof(float) * 2;  // 2 seconds buffer
    exinfo.numchannels = 1;  // Mono
    exinfo.defaultfrequency = _sampleRate;
    exinfo.format = FMOD_SOUND_FORMAT_PCMFLOAT;
    exinfo.pcmreadcallback = &FMODAudioSystem::PCMReadCallback;
    exinfo.userdata = this;  // Pass 'this' pointer to callback

    // Create streaming sound
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

    std::cout << "[FMOD] Initialized successfully" << std::endl;
    std::cout << "[FMOD] Sample Rate: " << _sampleRate << " Hz" << std::endl;
    std::cout << "[FMOD] Buffer Size: " << _bufferSize << " samples" << std::endl;
    std::cout << "[FMOD] Latency: ~" << (_bufferSize * 1000 / _sampleRate) << " ms" << std::endl;

    return true;
}

void FMODAudioSystem::Start() {
    if (!_initialized || _playing) {
        return;
    }

    FMOD_RESULT result = _system->playSound(_sound, nullptr, false, &_channel);
    if (result != FMOD_OK) {
        LogFMODError(result, "playSound");
        return;
    }

    // Set initial volume
    if (_channel) {
        _channel->setVolume(_volume);
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
    // Approximate latency based on buffer size
    return (_bufferSize * 1000) / _sampleRate;
}

FMODAudioSystem::AudioStats FMODAudioSystem::GetStats() const {
    AudioStats stats = {};

    if (_bus) {
        int bufferLevel = _bus->GetAudioBufferLevel();
        int maxBuffer = 8192;  // Assuming AUDIO_RINGBUFFER_SIZE
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

// Static callback - FMOD calls this when it needs samples
FMOD_RESULT F_CALL FMODAudioSystem::PCMReadCallback(
    FMOD_SOUND* sound,
    void* data,
    unsigned int datalen)
{
    // Get the FMODAudioSystem instance from userdata
    void* userdata = nullptr;
    FMOD::Sound* soundObj = (FMOD::Sound*)sound;
    soundObj->getUserData(&userdata);

    if (!userdata) {
        // No instance, fill with silence
        memset(data, 0, datalen);
        return FMOD_OK;
    }

    FMODAudioSystem* audioSystem = static_cast<FMODAudioSystem*>(userdata);
    return audioSystem->FillAudioBuffer(data, datalen);
}

// Instance method - fill the audio buffer
FMOD_RESULT FMODAudioSystem::FillAudioBuffer(void* data, unsigned int datalen) {
    if (!_bus) {
        memset(data, 0, datalen);
        return FMOD_OK;
    }

    float* samples = static_cast<float*>(data);
    int sampleCount = datalen / sizeof(float);

    // Pull samples from Bus ring buffer
    for (int i = 0; i < sampleCount; i++) {
        double sample;
        if (_bus->GetAudioSample(sample)) {
            // Clamp to prevent distortion
            sample = std::max(-1.0, std::min(1.0, sample));
            samples[i] = static_cast<float>(sample);
        }
        else {
            // Buffer underrun - use silence
            samples[i] = 0.0f;
            _underrunCount++;
        }
    }

    return FMOD_OK;
}

// Exports for Fmod Audio System
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
