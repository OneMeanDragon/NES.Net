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
public:
    APU2A03();
    ~APU2A03();

    void CpuWrite(uint16_t addr, uint8_t data);
    uint8_t CpuRead(uint16_t addr);
    void Clock();
    void Reset();

    double GetOutputSample();

    // Small inline helpers to configure/read the APU (no .cpp changes required)
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

    // Audio output/sample rate hint (not used by current .cpp but useful later)
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
    bool triangle_control_flag = false;          // control/halt flag (from 0x4008 bit 7)
    uint8_t triangle_linear_reload_value = 0;    // 7-bit reload value from 0x4008
    uint8_t triangle_linear_counter = 0;         // current linear counter
    bool triangle_linear_reload = false;         // linear reload request (set by writes)
    Sequencer triangle_seq;                      // use seq.reload/timer as the triangle timer
    uint8_t triangle_index = 0;                  // 0..31 waveform index
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
    bool dmc_enable = false;                 // controlled by 0x4015 bit 4
    bool dmc_irq_enable = false;             // 0x4010 bit 7
    bool dmc_loop = false;                   // 0x4010 bit 6 (looping)
    uint8_t dmc_rate_index = 0;              // 0x4010 low 4 bits
    uint16_t dmc_period = 428;               // timer period in CPU cycles
    uint8_t dmc_output_level = 0;            // current DAC output (0..127) from 0x4011 and delta adjustments
    uint32_t dmc_sample_address = 0;         // start address (0xC000 + 64*X) from 0x4012
    uint32_t dmc_sample_length = 0;          // sample length in bytes from 0x4013*16+1
    uint32_t dmc_current_address = 0;        // current fetch address
    uint32_t dmc_bytes_remaining = 0;        // remaining bytes in sample
    uint8_t dmc_sample_buffer = 0;           // buffered byte ready for shifting
    bool dmc_sample_buffer_empty = true;
    uint8_t dmc_shift_register = 0;          // shift register for current byte
    uint8_t dmc_bits_remaining = 0;          // bits remaining in shift register
    uint16_t dmc_timer = 0;                  // countdown timer

};
