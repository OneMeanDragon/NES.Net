#pragma once

#include <cstdint>
#include <cmath>

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif

// Forward declare Cartridge (defined in BusInterfaces.h / cartridge DLL)
class CartridgeInterfaceAPI;

// Oscillator for pulse wave generation
class OscPulse {
public:
    double frequency = 0.0;
    double dutycycle = 0.0;
    double amplitude = 1.0;
    double harmonics = 20.0;

    double Sample(double t);

private:
    static constexpr double PI = 3.14159265358979323846;
    static double ApproxSin(double t);
};

// Sequencer for timing
struct Sequencer {
    uint32_t sequence = 0;
    uint32_t new_sequence = 0;
    uint16_t timer = 0;
    uint16_t reload = 0;
    uint8_t output = 0;

    using Manipulator = void(*)(uint32_t& s);

    uint8_t Clock(bool enable, Manipulator func);
};

// Envelope generator
class Envelope {
public:
    bool start = false;
    bool disable = false;
    uint16_t divider_count = 0;
    uint16_t volume = 0;
    uint16_t output = 0;
    uint16_t decay_count = 0;

    void Clock(bool loop);
};

// Length counter
struct LengthCounter {
    uint8_t counter = 0;

    uint8_t Clock(bool enable, bool halt);
};

// Frequency sweeper
class Sweeper {
public:
    bool enabled = false;
    bool down = false;
    bool reload = false;
    uint8_t shift = 0;
    uint8_t timer = 0;
    uint8_t period = 0;
    uint16_t change = 0;
    bool mute = false;

    void Track(uint16_t& target);
    bool Clock(uint16_t& target, bool channel);
};

// Main APU class
class APU2A03 {
private:
    bool _poweron = false;
public:
    void PowerOff() { _poweron = false; }
    bool Powered() const { return _poweron; }
    void InitialState();
public:
    APU2A03();
    ~APU2A03();

    void CpuWrite(uint16_t addr, uint8_t data);
    uint8_t CpuRead(uint16_t addr);
    void Clock();
    void Reset();

    double GetOutputSample();
    bool IsIRQActive() const;

    // Small inline helpers to configure/read the APU
    inline void SetRawMode(bool raw) { use_raw_mode = raw; }
    inline bool GetRawMode() const { return use_raw_mode; }
    inline void SetSampleRate(double rate) { sample_rate = rate; }
    inline double GetSampleRate() const { return sample_rate; }

    // Provide CartridgeInterface pointer so DMC can fetch sample bytes (non-owning)
    void SetCartridge(CartridgeInterfaceAPI* cart);

    // Visual feedback (for debugging)
    uint16_t pulse1_visual = 0;
    uint16_t pulse2_visual = 0;
    uint16_t noise_visual = 0;
    uint16_t triangle_visual = 0;

    // Accessors for visuals
    inline uint16_t GetPulse1Visual() const { return pulse1_visual; }
    inline uint16_t GetPulse2Visual() const { return pulse2_visual; }
    inline uint16_t GetNoiseVisual() const { return noise_visual; }
    inline uint16_t GetTriangleVisual() const { return triangle_visual; }

private:
    static constexpr uint8_t length_table[32] = {
        10, 254, 20, 2, 40, 4, 80, 6,
        160, 8, 60, 10, 14, 12, 26, 14,
        12, 16, 24, 18, 48, 20, 96, 22,
        192, 24, 72, 26, 16, 28, 32, 30
    };

    uint32_t frame_clock_counter = 0;
    uint32_t clock_counter = 0;
    bool use_raw_mode = false;
    double global_time = 0.0;

    // Audio output/sample rate hint
    double sample_rate = 44100.0;

    // CartridgeInterface pointer for DMC memory fetches (non-owning, set by bus)
    CartridgeInterfaceAPI* _cart = nullptr;

    // Pulse 1
    bool pulse1_enable = false;
    bool pulse1_halt = false;
    double pulse1_sample = 0.0;
    double pulse1_output = 0.0;
    Sequencer pulse1_seq;
    OscPulse pulse1_osc;
    Envelope pulse1_env;
    LengthCounter pulse1_lc;
    Sweeper pulse1_sweep;

    // Pulse 2
    bool pulse2_enable = false;
    bool pulse2_halt = false;
    double pulse2_sample = 0.0;
    double pulse2_output = 0.0;
    Sequencer pulse2_seq;
    OscPulse pulse2_osc;
    Envelope pulse2_env;
    LengthCounter pulse2_lc;
    Sweeper pulse2_sweep;

    // Triangle
    bool triangle_enable = false;
    bool triangle_control_flag = false;
    uint8_t triangle_linear_reload_value = 0;
    uint8_t triangle_linear_counter = 0;
    bool triangle_linear_reload = false;
    Sequencer triangle_seq;
    uint8_t triangle_index = 0;
    LengthCounter triangle_lc;
    double triangle_sample = 0.0;
    double triangle_output = 0.0;

    // Noise
    bool noise_enable = false;
    bool noise_halt = false;
    Envelope noise_env;
    LengthCounter noise_lc;
    Sequencer noise_seq;
    double noise_sample = 0.0;
    double noise_output = 0.0;

    // DMC (Delta Modulation Channel)
    bool dmc_enable = false;
    bool dmc_irq_enable = false;
    bool dmc_irq_flag = false;
    bool dmc_loop = false;
    uint8_t dmc_rate_index = 0;
    uint16_t dmc_period = 428;
    uint8_t dmc_output_level = 0;
    uint32_t dmc_sample_address = 0;
    uint32_t dmc_sample_length = 0;
    uint32_t dmc_current_address = 0;
    uint32_t dmc_bytes_remaining = 0;
    uint8_t dmc_sample_buffer = 0;
    bool dmc_sample_buffer_empty = true;
    uint8_t dmc_shift_register = 0;
    uint8_t dmc_bits_remaining = 0;
    uint16_t dmc_timer = 0;

    // Frame Counter
    bool frame_counter_mode = false;              // false = 4-step, true = 5-step
    bool frame_counter_irq_disable = false;
    bool frame_counter_irq_flag = false;
    uint8_t frame_counter_reset_delay = 0;
    bool frame_counter_should_clock_immediately = false;

    // Frame counter helper functions
    void ClockFrameCounter();
    void ClockQuarterFrame();
    void ClockHalfFrame();
    void ClockDMC();

private: // non owning
    class NESBus* _bus = nullptr;
public:
    void ConnectBus(class NESBus* bus) { _bus = bus; }
};

// Exports
DLLEXPORT APU2A03* CreateAPU();
DLLEXPORT void DestroyAPU(APU2A03* apu);

DLLEXPORT void APU_CpuWrite(APU2A03* apu, uint16_t addr, uint8_t data);
DLLEXPORT uint8_t APU_CpuRead(APU2A03* apu, uint16_t addr);
DLLEXPORT void APU_Clock(APU2A03* apu);
DLLEXPORT void APU_Reset(APU2A03* apu);
DLLEXPORT double APU_GetOutputSample(APU2A03* apu);
DLLEXPORT bool APU_IsIRQActive(APU2A03* apu);

// Aliases
DLLEXPORT void ClockAPU(APU2A03* apu);
DLLEXPORT void ResetAPU(APU2A03* apu);
