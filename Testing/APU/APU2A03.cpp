//#define RELEASE_CARTRIDGE
#include "APU2A03.h"
#include <algorithm>
#if defined(RELEASE_CARTRIDGE)
#include "CartridgeApi/CartridgeInterfaceAPI.h"
#else
#include "../DiagnosticCartridge.h"
#endif

// DMC period table for NTSC (in CPU cycles)
static const uint16_t DMC_PERIODS[16] = {
    428, 380, 340, 320, 286, 254, 226, 214,
    190, 160, 142, 128, 106, 84,  72,  54
};

// Triangle 32-step sequence (0..15..0..15)
static const uint8_t TRIANGLE_SEQ[32] = {
    15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,
     0, 1, 2, 3, 4, 5,6,7,8,9,10,11,12,13,14,15
};

// OscPulse implementation
double OscPulse::ApproxSin(double t) {
    double j = t * 0.15915;
    j = j - std::floor(j);
    return 20.785 * j * (j - 0.5) * (j - 1.0);
}

double OscPulse::Sample(double t) {
    double a = 0.0;
    double b = 0.0;
    double p = dutycycle * 2.0 * PI;

    for (double n = 1.0; n < harmonics; n += 1.0) {
        double c = n * frequency * 2.0 * PI * t;
        a += -ApproxSin(c) / n;
        b += -ApproxSin(c - p * n) / n;
    }

    return (2.0 * amplitude / PI) * (a - b);
}

// Sequencer implementation
uint8_t Sequencer::Clock(bool enable, Manipulator func) {
    if (enable) {
        if (timer == 0) {
            timer = reload;
            if (func) {
                func(sequence);
            }
            output = static_cast<uint8_t>(sequence & 0x1);
        }
        else {
            timer--;
        }
    }
    return output;
}

// Envelope implementation
void Envelope::Clock(bool loop) {
    if (!start) {
        if (divider_count == 0) {
            divider_count = volume;
            if (decay_count == 0) {
                if (loop) {
                    decay_count = 15;
                }
            }
            else {
                decay_count--;
            }
        }
        else {
            divider_count--;
        }
    }
    else {
        start = false;
        decay_count = 15;
        divider_count = volume;
    }

    if (disable) {
        output = volume;
    }
    else {
        output = decay_count;
    }
}

// LengthCounter implementation
uint8_t LengthCounter::Clock(bool enable, bool halt) {
    if (!enable) {
        counter = 0;
    }
    else {
        if (counter > 0 && !halt) {
            counter--;
        }
    }
    return counter;
}

// Sweeper implementation
void Sweeper::Track(uint16_t& target) {
    if (enabled) {
        change = target >> shift;
        mute = (target < 8) || (target > 0x7FF);
    }
}

bool Sweeper::Clock(uint16_t& target, bool channel) {
    bool changed = false;

    if (timer == 0 && enabled && shift > 0 && !mute) {
        if (target >= 8 && change < 0x7FF) {
            if (down) {
                target -= (change - (channel ? 1 : 0));
            }
            else {
                target += change;
            }
            changed = true;
        }
    }

    if (timer == 0 || reload) {
        timer = period;
        reload = false;
    }
    else {
        timer--;
    }

    mute = (target < 8) || (target > 0x7FF);

    return changed;
}

// APU2A03 implementation
APU2A03::APU2A03() : _cart(nullptr), _bus(nullptr) {
    InitialState();
}

APU2A03::~APU2A03() {
    // non-owning _cart and _bus — do not delete
}

void APU2A03::SetCartridge(CartridgeInterfaceAPI* cart) {
    _cart = cart;
}

void APU2A03::CpuWrite(uint16_t addr, uint8_t data) {
    switch (addr) {
        // Pulse 1
    case 0x4000:
        switch ((data & 0xC0) >> 6) {
        case 0x0: pulse1_seq.new_sequence = 0b10000000; pulse1_osc.dutycycle = 0.125; break;
        case 0x1: pulse1_seq.new_sequence = 0b11000000; pulse1_osc.dutycycle = 0.25; break;
        case 0x2: pulse1_seq.new_sequence = 0b11110000; pulse1_osc.dutycycle = 0.5; break;
        case 0x3: pulse1_seq.new_sequence = 0b10011111; pulse1_osc.dutycycle = 0.75; break;
        }
        pulse1_seq.sequence = pulse1_seq.new_sequence;
        pulse1_halt = (data & 0x20) != 0;
        pulse1_env.volume = (data & 0x0F);
        pulse1_env.disable = (data & 0x10) != 0;
        break;

    case 0x4001:
        pulse1_sweep.enabled = (data & 0x80) != 0;
        pulse1_sweep.period = (data & 0x70) >> 4;
        pulse1_sweep.down = (data & 0x08) != 0;
        pulse1_sweep.shift = (data & 0x07);
        pulse1_sweep.reload = true;
        break;

    case 0x4002:
        pulse1_seq.reload = (pulse1_seq.reload & 0xFF00) | data;
        break;

    case 0x4003:
        pulse1_seq.reload = static_cast<uint16_t>((data & 0x07) << 8) | (pulse1_seq.reload & 0x00FF);
        pulse1_seq.timer = pulse1_seq.reload;
        pulse1_seq.sequence = pulse1_seq.new_sequence;
        pulse1_lc.counter = length_table[(data & 0xF8) >> 3];
        pulse1_env.start = true;
        break;

        // Pulse 2
    case 0x4004:
        switch ((data & 0xC0) >> 6) {
        case 0x0: pulse2_seq.new_sequence = 0b10000000; pulse2_osc.dutycycle = 0.125; break;
        case 0x1: pulse2_seq.new_sequence = 0b11000000; pulse2_osc.dutycycle = 0.25; break;
        case 0x2: pulse2_seq.new_sequence = 0b11110000; pulse2_osc.dutycycle = 0.5; break;
        case 0x3: pulse2_seq.new_sequence = 0b10011111; pulse2_osc.dutycycle = 0.75; break;
        }
        pulse2_seq.sequence = pulse2_seq.new_sequence;
        pulse2_halt = (data & 0x20) != 0;
        pulse2_env.volume = (data & 0x0F);
        pulse2_env.disable = (data & 0x10) != 0;
        break;

    case 0x4005:
        pulse2_sweep.enabled = (data & 0x80) != 0;
        pulse2_sweep.period = (data & 0x70) >> 4;
        pulse2_sweep.down = (data & 0x08) != 0;
        pulse2_sweep.shift = (data & 0x07);
        pulse2_sweep.reload = true;
        break;

    case 0x4006:
        pulse2_seq.reload = (pulse2_seq.reload & 0xFF00) | data;
        break;

    case 0x4007:
        pulse2_seq.reload = static_cast<uint16_t>((data & 0x07) << 8) | (pulse2_seq.reload & 0x00FF);
        pulse2_seq.timer = pulse2_seq.reload;
        pulse2_seq.sequence = pulse2_seq.new_sequence;
        pulse2_lc.counter = length_table[(data & 0xF8) >> 3];
        pulse2_env.start = true;
        break;

        // Triangle
    case 0x4008:
        triangle_control_flag = (data & 0x80) != 0;
        triangle_linear_reload_value = data & 0x7F;
        break;

    case 0x4009:
        // Unused
        break;

    case 0x400A:
        triangle_seq.reload = (triangle_seq.reload & 0xFF00) | data;
        break;

    case 0x400B:
        triangle_seq.reload = static_cast<uint16_t>((data & 0x07) << 8) | (triangle_seq.reload & 0x00FF);
        triangle_seq.timer = triangle_seq.reload;
        triangle_lc.counter = length_table[(data & 0xF8) >> 3];
        triangle_linear_reload = true;
        break;

        // Noise
    case 0x400C:
        noise_env.volume = (data & 0x0F);
        noise_env.disable = (data & 0x10) != 0;
        noise_halt = (data & 0x20) != 0;
        break;

    case 0x400E:
        switch (data & 0x0F) {
        case 0x0: noise_seq.reload = 0; break;
        case 0x1: noise_seq.reload = 4; break;
        case 0x2: noise_seq.reload = 8; break;
        case 0x3: noise_seq.reload = 16; break;
        case 0x4: noise_seq.reload = 32; break;
        case 0x5: noise_seq.reload = 64; break;
        case 0x6: noise_seq.reload = 96; break;
        case 0x7: noise_seq.reload = 128; break;
        case 0x8: noise_seq.reload = 160; break;
        case 0x9: noise_seq.reload = 202; break;
        case 0xA: noise_seq.reload = 254; break;
        case 0xB: noise_seq.reload = 380; break;
        case 0xC: noise_seq.reload = 508; break;
        case 0xD: noise_seq.reload = 1016; break;
        case 0xE: noise_seq.reload = 2034; break;
        case 0xF: noise_seq.reload = 4068; break;
        }
        break;

    case 0x400F:
        noise_lc.counter = length_table[(data & 0xF8) >> 3];
        noise_env.start = true;
        break;

        // DMC
    case 0x4010:
        dmc_irq_enable = (data >> 7) != 0;
        dmc_loop = (data >> 6) != 0;
        dmc_rate_index = (data & 0x0F);
        dmc_period = DMC_PERIODS[dmc_rate_index];
        dmc_timer = dmc_period;

        // Clear DMC IRQ flag if IRQ is disabled
        if (!dmc_irq_enable) {
            dmc_irq_flag = false;
        }
        break;

    case 0x4011:
        dmc_output_level = data & 0x7F;
        break;

    case 0x4012:
        dmc_sample_address = 0xC000 | (static_cast<uint32_t>(data) << 6);
        break;

    case 0x4013:
        dmc_sample_length = (static_cast<uint32_t>(data) << 4) | 1;
        break;

        // Status
    case 0x4015:
        pulse1_enable = (data & 0x01) != 0;
        pulse2_enable = (data & 0x02) != 0;
        triangle_enable = (data & 0x04) != 0;
        noise_enable = (data & 0x08) != 0;

        // Disabling a channel immediately clears its length counter
        if (!pulse1_enable) pulse1_lc.counter = 0;
        if (!pulse2_enable) pulse2_lc.counter = 0;
        if (!triangle_enable) triangle_lc.counter = 0;
        if (!noise_enable) noise_lc.counter = 0;

        // DMC
        {
            bool new_dmc_enable = (data & 0x10) != 0;
            if (!dmc_enable && new_dmc_enable && dmc_bytes_remaining == 0) {
                dmc_current_address = dmc_sample_address;
                dmc_bytes_remaining = dmc_sample_length;
            }
            dmc_enable = new_dmc_enable;
            if (!dmc_enable) {
                dmc_bytes_remaining = 0;
            }

            // Clear DMC interrupt flag
            dmc_irq_flag = false;
        }
        break;

    case 0x4017: // Frame Counter
        frame_counter_mode = (data & 0x80) != 0;  // 0 = 4-step, 1 = 5-step
        frame_counter_irq_disable = (data & 0x40) != 0;

        // If IRQ disable is set, clear the frame interrupt flag
        if (frame_counter_irq_disable) {
            frame_counter_irq_flag = false;
        }

        // Reset is delayed by 3 or 4 CPU cycles
        frame_counter_reset_delay = 3;

        // If 5-step mode, clock immediately (after delay)
        if (frame_counter_mode) {
            frame_counter_should_clock_immediately = true;
        }
        break;
    }
}

uint8_t APU2A03::CpuRead(uint16_t addr) {
    uint8_t data = 0xFF;

    if (addr == 0x4015) {
        // Bit 0-3: Length counter status
        data |= (pulse1_lc.counter > 0) ? 0x01 : 0x00;
        data |= (pulse2_lc.counter > 0) ? 0x02 : 0x00;
        data |= (triangle_lc.counter > 0) ? 0x04 : 0x00;
        data |= (noise_lc.counter > 0) ? 0x08 : 0x00;

        // Bit 4: DMC active
        data |= (dmc_bytes_remaining > 0) ? 0x10 : 0x00;

        // Bit 6: Frame interrupt flag
        data |= frame_counter_irq_flag ? 0x40 : 0x00;

        // Bit 7: DMC interrupt flag
        data |= dmc_irq_flag ? 0x80 : 0x00;

        // Reading $4015 clears the frame interrupt flag
        frame_counter_irq_flag = false;
    }

    return data;
}

void APU2A03::ClockFrameCounter() {
    // Handle reset delay
    if (frame_counter_reset_delay > 0) {
        frame_counter_reset_delay--;
        if (frame_counter_reset_delay == 0) {
            frame_clock_counter = 0;

            // If 5-step mode and should clock immediately
            if (frame_counter_should_clock_immediately) {
                ClockQuarterFrame();
                ClockHalfFrame();
                frame_counter_should_clock_immediately = false;
            }
        }
        return;
    }

    bool quarter_frame_clock = false;
    bool half_frame_clock = false;

    if (frame_counter_mode) {
        // 5-step mode
        switch (frame_clock_counter) {
        case 3728:
            quarter_frame_clock = true;
            break;
        case 7456:
            quarter_frame_clock = true;
            half_frame_clock = true;
            break;
        case 11185:
            quarter_frame_clock = true;
            break;
        case 14914:
            quarter_frame_clock = true;
            half_frame_clock = true;
            break;
        case 18640:
            frame_clock_counter = 0;
            return;
        }
    }
    else {
        // 4-step mode
        switch (frame_clock_counter) {
        case 3728:
            quarter_frame_clock = true;
            break;
        case 7456:
            quarter_frame_clock = true;
            half_frame_clock = true;
            break;
        case 11185:
            quarter_frame_clock = true;
            break;
        case 14914:
            quarter_frame_clock = true;
            half_frame_clock = true;

            // Set frame IRQ if not inhibited
            if (!frame_counter_irq_disable) {
                frame_counter_irq_flag = true;
            }
            break;
        case 14915:
            // IRQ flag stays set
            if (!frame_counter_irq_disable) {
                frame_counter_irq_flag = true;
            }
            frame_clock_counter = 0;
            return;
        }
    }

    if (quarter_frame_clock) {
        ClockQuarterFrame();
    }

    if (half_frame_clock) {
        ClockHalfFrame();
    }
}

void APU2A03::ClockQuarterFrame() {
    // Clock envelopes
    pulse1_env.Clock(pulse1_halt);
    pulse2_env.Clock(pulse2_halt);
    noise_env.Clock(noise_halt);

    // Triangle linear counter
    if (triangle_linear_reload) {
        triangle_linear_counter = triangle_linear_reload_value;
    }
    else if (triangle_linear_counter > 0) {
        triangle_linear_counter--;
    }

    if (!triangle_control_flag) {
        triangle_linear_reload = false;
    }
}

void APU2A03::ClockHalfFrame() {
    // Clock length counters
    pulse1_lc.Clock(pulse1_enable, pulse1_halt);
    pulse2_lc.Clock(pulse2_enable, pulse2_halt);
    triangle_lc.Clock(triangle_enable, triangle_control_flag);
    noise_lc.Clock(noise_enable, noise_halt);

    // Clock sweeps
    pulse1_sweep.Clock(pulse1_seq.reload, false);
    pulse2_sweep.Clock(pulse2_seq.reload, true);
}

void APU2A03::Clock() {
    // Increment global time
    global_time += (0.3333333333 / 1789773.0);

    // APU runs at half CPU speed (every other CPU cycle)
    if (clock_counter % 6 == 0) {
        frame_clock_counter++;
        ClockFrameCounter();

        // Update Pulse 1
        pulse1_seq.Clock(pulse1_enable, [](uint32_t& s) {
            s = ((s & 0x0001) << 7) | ((s & 0x00FE) >> 1);
            });

        pulse1_osc.frequency = 1789773.0 / (16.0 * (static_cast<double>(pulse1_seq.reload) + 1.0));
        pulse1_osc.amplitude = (static_cast<double>(pulse1_env.output) - 1.0) / 16.0;
        pulse1_sample = pulse1_osc.Sample(global_time);

        if (pulse1_lc.counter > 0 && pulse1_seq.timer >= 8 && !pulse1_sweep.mute && pulse1_env.output > 2) {
            pulse1_output += (pulse1_sample - pulse1_output) * 0.5;
        }
        else {
            pulse1_output = 0.0;
        }

        // Update Pulse 2
        pulse2_seq.Clock(pulse2_enable, [](uint32_t& s) {
            s = ((s & 0x0001) << 7) | ((s & 0x00FE) >> 1);
            });

        pulse2_osc.frequency = 1789773.0 / (16.0 * (static_cast<double>(pulse2_seq.reload) + 1.0));
        pulse2_osc.amplitude = (static_cast<double>(pulse2_env.output) - 1.0) / 16.0;
        pulse2_sample = pulse2_osc.Sample(global_time);

        if (pulse2_lc.counter > 0 && pulse2_seq.timer >= 8 && !pulse2_sweep.mute && pulse2_env.output > 2) {
            pulse2_output += (pulse2_sample - pulse2_output) * 0.5;
        }
        else {
            pulse2_output = 0.0;
        }

        // Update Triangle
        if (triangle_enable) {
            if (triangle_seq.timer == 0) {
                triangle_seq.timer = triangle_seq.reload;
                triangle_index = static_cast<uint8_t>((triangle_index + 1) & 0x1F);
            }
            else {
                triangle_seq.timer--;
            }

            if (triangle_lc.counter > 0 && triangle_linear_counter > 0) {
                triangle_sample = static_cast<double>(TRIANGLE_SEQ[triangle_index]) / 15.0;
                triangle_output += (triangle_sample - triangle_output) * 0.5;
            }
            else {
                triangle_output = 0.0;
            }
        }
        else {
            triangle_output = 0.0;
        }

        // Update Noise
        noise_seq.Clock(noise_enable, [](uint32_t& s) {
            s = (((s & 0x0001) ^ ((s & 0x0002) >> 1)) << 14) | ((s & 0x7FFF) >> 1);
            });

        if (noise_lc.counter > 0 && noise_seq.timer >= 8) {
            noise_output = static_cast<double>(noise_seq.output) *
                ((static_cast<double>(noise_env.output) - 1.0) / 16.0);
        }
        else {
            noise_output = 0.0;
        }

        // Force silence if disabled
        if (!pulse1_enable) pulse1_output = 0.0;
        if (!pulse2_enable) pulse2_output = 0.0;
        if (!noise_enable) noise_output = 0.0;
        if (!triangle_enable) triangle_output = 0.0;
    }

    // Track sweepers every cycle
    pulse1_sweep.Track(pulse1_seq.reload);
    pulse2_sweep.Track(pulse2_seq.reload);

    // DMC (runs every CPU cycle)
    ClockDMC();

    // Visual feedback
    pulse1_visual = (pulse1_enable && pulse1_env.output > 1 && !pulse1_sweep.mute) ?
        pulse1_seq.reload : 2047;
    pulse2_visual = (pulse2_enable && pulse2_env.output > 1 && !pulse2_sweep.mute) ?
        pulse2_seq.reload : 2047;
    noise_visual = (noise_enable && noise_env.output > 1) ?
        pulse1_seq.reload : 2047;
    triangle_visual = (triangle_enable && triangle_linear_counter > 0) ? triangle_seq.reload : 2047;

    clock_counter++;
}

void APU2A03::ClockDMC() {
    if (!dmc_enable) return;

    if (dmc_timer == 0) {
        dmc_timer = dmc_period;

        if (dmc_bits_remaining == 0) {
            if (!dmc_sample_buffer_empty) {
                dmc_shift_register = dmc_sample_buffer;
                dmc_bits_remaining = 8;
                dmc_sample_buffer_empty = true;
            }
            else if (dmc_bytes_remaining > 0) {
                // Fetch next byte
                uint8_t fetched = 0x00;
                if (_cart != nullptr) {
                    _cart->CpuRead(static_cast<uint16_t>(dmc_current_address & 0xFFFF), &fetched);
                }
                dmc_sample_buffer = fetched;
                dmc_sample_buffer_empty = false;

                dmc_bytes_remaining--;
                dmc_current_address++;
                if (dmc_current_address > 0xFFFF) dmc_current_address = 0x8000;

                if (dmc_bytes_remaining == 0) {
                    if (dmc_loop) {
                        dmc_current_address = dmc_sample_address;
                        dmc_bytes_remaining = dmc_sample_length;
                    }
                    else if (dmc_irq_enable) {
                        dmc_irq_flag = true;
                    }
                }
            }
        }

        if (dmc_bits_remaining > 0) {
            if (dmc_shift_register & 1) {
                if (dmc_output_level <= 125) dmc_output_level += 2;
            }
            else {
                if (dmc_output_level >= 2) dmc_output_level -= 2;
            }
            dmc_shift_register >>= 1;
            dmc_bits_remaining--;
        }
    }
    else {
        dmc_timer--;
    }
}

bool APU2A03::IsIRQActive() const {
    return frame_counter_irq_flag || dmc_irq_flag;
}

double APU2A03::GetOutputSample() {
    // Test tone for measuring latency
    if (_testToneEnabled) {
        // 440 Hz sine wave
        double freq = 440.0;
        _testTonePhase += (2.0 * 3.14159265358979323846 * freq) / sample_rate;
        if (_testTonePhase > 2.0 * 3.14159265358979323846) {
            _testTonePhase -= 2.0 * 3.14159265358979323846;
        }
        return 0.5 * std::sin(_testTonePhase);
    }

    if (use_raw_mode) {
        return (pulse1_sample - 0.5) * 0.5 + (pulse2_sample - 0.5) * 0.5;
    }
    else {
        double pulse_mix =
            ((1.0 * pulse1_output) - 0.8) * 0.1 +
            ((1.0 * pulse2_output) - 0.8) * 0.1;

        double triangle_mix = ((triangle_output)-0.5) * 0.15;
        double noise_mix = ((2.0 * (noise_output - 0.5))) * 0.1;
        double dmc_mix = ((static_cast<double>(dmc_output_level) / 127.0) - 0.5) * 0.15;

        return pulse_mix + triangle_mix + noise_mix + dmc_mix;
    }
}

void APU2A03::InitialState() {
    noise_seq.sequence = 0xDBDB;

    // Initialize triangle state
    triangle_seq.sequence = 0;
    triangle_seq.reload = 0;
    triangle_seq.timer = 0;
    triangle_index = 0;
    triangle_linear_counter = 0;
    triangle_linear_reload = false;
    triangle_control_flag = false;

    // Initialize DMC state
    dmc_enable = false;
    dmc_irq_enable = false;
    dmc_irq_flag = false;
    dmc_loop = false;
    dmc_rate_index = 0;
    dmc_period = DMC_PERIODS[0];
    dmc_output_level = 0;
    dmc_sample_address = 0;
    dmc_sample_length = 0;
    dmc_current_address = 0;
    dmc_bytes_remaining = 0;
    dmc_sample_buffer = 0;
    dmc_sample_buffer_empty = true;
    dmc_shift_register = 0;
    dmc_bits_remaining = 0;
    dmc_timer = dmc_period;

    // Initialize frame counter
    frame_counter_mode = false;  // 4-step mode
    frame_counter_irq_disable = false;
    frame_counter_irq_flag = false;
    frame_counter_reset_delay = 0;
    frame_counter_should_clock_immediately = false;

}

void APU2A03::Reset(bool coldstart) {
    if (coldstart) { // cartridge was just inserted so the power wasent on yet..
        InitialState();
        return;
    }
    // 1. Clear frame counter position
    frame_clock_counter = 0;

    // 2. Disable all channels ($4015 = 0)
    pulse1_enable = false;
    pulse2_enable = false;
    triangle_enable = false;
    noise_enable = false;
    dmc_enable = false;

    // 3. Clear all length counters
    pulse1_lc.counter = 0;
    pulse2_lc.counter = 0;
    triangle_lc.counter = 0;
    noise_lc.counter = 0;

    // 4. Reset envelopes
    // (Set to initial state)

    // 5. Clear IRQ flags
    frame_counter_irq_flag = false;
    dmc_irq_flag = false;

    // 6. Reset DMC to default state
    dmc_sample_address = 0xC000;
    dmc_sample_length = 1;
    dmc_bytes_remaining = 0;

    // 7. DO NOT clear $4017 mode bits
    // frame_counter_mode - UNCHANGED
    // frame_counter_irq_disable - UNCHANGED
}


// C exports
DLLEXPORT APU2A03* CreateAPU() {
    return new APU2A03();
}

DLLEXPORT void DestroyAPU(APU2A03* apu) {
    delete apu;
}

DLLEXPORT void APU_CpuWrite(APU2A03* apu, uint16_t addr, uint8_t data) {
    if (apu) apu->CpuWrite(addr, data);
}

DLLEXPORT uint8_t APU_CpuRead(APU2A03* apu, uint16_t addr) {
    if (apu) return apu->CpuRead(addr);
    return 0;
}

DLLEXPORT void APU_Clock(APU2A03* apu) {
    if (apu) apu->Clock();
}

DLLEXPORT void APU_Reset(APU2A03* apu, bool coldstart) {
    if (apu) apu->Reset(coldstart);
}

DLLEXPORT double APU_GetOutputSample(APU2A03* apu) {
    if (apu) return apu->GetOutputSample();
    return 0.0;
}

DLLEXPORT bool APU_IsIRQActive(APU2A03* apu) {
    if (apu) return apu->IsIRQActive();
    return false;
}

// Aliases for Bus compatibility
DLLEXPORT void ClockAPU(APU2A03* apu) {
    if (apu) apu->Clock();
}

DLLEXPORT void ResetAPU(APU2A03* apu, bool coldstart) {
    if (apu) apu->Reset(coldstart);
}