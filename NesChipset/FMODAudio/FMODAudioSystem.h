#pragma once
#pragma comment(lib, "fmod_vc.lib")

#include <cstdint>
#include <fmod.hpp>
#include <fmod_errors.h>

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif

// Forward declaration
class NESBus;

class FMODAudioSystem {
public:
    FMODAudioSystem();
    ~FMODAudioSystem();

    // Initialize FMOD with NES audio specs
    bool Initialize(NESBus* bus, int sampleRate = 44100, int bufferSize = 512);

    // Start/Stop playback
    void Start();
    void Stop();
    void Pause(bool pause);

    // Update (call once per frame)
    void Update();

    // Configuration
    void SetVolume(float volume);  // 0.0 to 1.0
    float GetVolume() const;

    // Status
    bool IsPlaying() const;
    int GetLatency() const;  // in milliseconds

    // Diagnostics
    struct AudioStats {
        int bufferFillPercent;
        int underrunCount;
        float cpuUsage;
        int latencyMs;
    };
    AudioStats GetStats() const;

private:
    // FMOD callback - called when FMOD needs audio samples
    static FMOD_RESULT F_CALL PCMReadCallback(FMOD_SOUND* sound, void* data, unsigned int datalen);

    // Instance callback (non-static)
    FMOD_RESULT FillAudioBuffer(void* data, unsigned int datalen);

    // FMOD objects
    FMOD::System* _system;
    FMOD::Sound* _sound;
    FMOD::Channel* _channel;

    // Bus reference (non-owning)
    NESBus* _bus;

    // Configuration
    int _sampleRate;
    int _bufferSize;
    float _volume;

    // State
    bool _initialized;
    bool _playing;

    // Statistics
    mutable int _underrunCount;

    // Helper
    void LogFMODError(FMOD_RESULT result, const char* function);
};

// Exports for Fmod Audio System
DLLEXPORT FMODAudioSystem* CreateFMODAudio();
DLLEXPORT void DestroyFMODAudio(FMODAudioSystem* audio);

DLLEXPORT bool FMODAudio_Initialize(FMODAudioSystem* audio, NESBus* bus, int sampleRate, int bufferSize);
DLLEXPORT void FMODAudio_Start(FMODAudioSystem* audio);
DLLEXPORT void FMODAudio_Stop(FMODAudioSystem* audio);
DLLEXPORT void FMODAudio_Pause(FMODAudioSystem* audio, bool pause);
DLLEXPORT void FMODAudio_Update(FMODAudioSystem* audio);

DLLEXPORT void FMODAudio_SetVolume(FMODAudioSystem* audio, float volume);
DLLEXPORT float FMODAudio_GetVolume(FMODAudioSystem* audio);

DLLEXPORT bool FMODAudio_IsPlaying(FMODAudioSystem* audio);
DLLEXPORT int FMODAudio_GetLatency(FMODAudioSystem* audio);