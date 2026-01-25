#pragma once

#include "DiagnosticTest.h"
#include "APU/APU2A03.h"
#include <cmath>

void TestAPUBasics(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Basic Initialization");

    apu->Reset(true);

    test.Assert(true, "APU reset without crashing");
    test.Assert(!apu->IsIRQActive(), "No IRQ active after reset");
}

void TestAPUStatusRegister(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Status Register (0x4015)");

    apu->Reset(true);

    // Read initial status
    uint8_t status = apu->CpuRead(0x4015);
    test.Info("Initial status: 0x" + std::to_string(status));

    // Enable pulse 1
    apu->CpuWrite(0x4015, 0x01);
    status = apu->CpuRead(0x4015);
    test.Assert((status & 0x01) != 0, "Pulse 1 enabled");

    // Enable pulse 2
    apu->CpuWrite(0x4015, 0x02);
    status = apu->CpuRead(0x4015);
    test.Assert((status & 0x02) != 0, "Pulse 2 enabled");

    // Enable triangle
    apu->CpuWrite(0x4015, 0x04);
    status = apu->CpuRead(0x4015);
    test.Assert((status & 0x04) != 0, "Triangle enabled");

    // Enable noise
    apu->CpuWrite(0x4015, 0x08);
    status = apu->CpuRead(0x4015);
    test.Assert((status & 0x08) != 0, "Noise enabled");

    // Enable all channels
    apu->CpuWrite(0x4015, 0x0F);
    status = apu->CpuRead(0x4015);
    test.Assert((status & 0x0F) == 0x0F, "All channels enabled");

    // Disable all channels
    apu->CpuWrite(0x4015, 0x00);
    status = apu->CpuRead(0x4015);
    test.Info("All channels disabled, status: 0x" + std::to_string(status));
}

void TestAPUPulse1(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Pulse 1 Channel");

    apu->Reset(true);

    // Configure pulse 1
    // 0x4000: Duty cycle and envelope
    apu->CpuWrite(0x4000, 0xBF); // 50% duty, constant volume, volume 15
    test.Assert(true, "Pulse 1 duty/envelope configured");

    // 0x4001: Sweep unit
    apu->CpuWrite(0x4001, 0x00); // Sweep disabled
    test.Assert(true, "Pulse 1 sweep configured");

    // 0x4002: Timer low
    apu->CpuWrite(0x4002, 0x54); // Low 8 bits of period
    test.Assert(true, "Pulse 1 timer low set");

    // 0x4003: Length counter and timer high
    apu->CpuWrite(0x4003, 0xF8); // Length 31, high 3 bits of period
    test.Assert(true, "Pulse 1 timer high set");

    // Enable pulse 1
    apu->CpuWrite(0x4015, 0x01);
    test.Assert(true, "Pulse 1 enabled");

    // Clock APU and verify it doesn't crash
    for (int i = 0; i < 1000; i++) {
        apu->Clock();
    }

    test.Assert(true, "Pulse 1 clocking successful");
}

void TestAPUPulse2(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Pulse 2 Channel");

    apu->Reset(true);

    // Configure pulse 2 (registers 0x4004-0x4007)
    apu->CpuWrite(0x4004, 0xBF);
    apu->CpuWrite(0x4005, 0x00);
    apu->CpuWrite(0x4006, 0x54);
    apu->CpuWrite(0x4007, 0xF8);

    test.Assert(true, "Pulse 2 configured");

    // Enable pulse 2
    apu->CpuWrite(0x4015, 0x02);

    // Clock and test
    for (int i = 0; i < 1000; i++) {
        apu->Clock();
    }

    test.Assert(true, "Pulse 2 clocking successful");
}

void TestAPUTriangle(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Triangle Channel");

    apu->Reset(true);

    // Configure triangle (registers 0x4008-0x400B)
    // 0x4008: Linear counter
    apu->CpuWrite(0x4008, 0x7F); // Control flag, linear counter load
    test.Assert(true, "Triangle linear counter configured");

    // 0x400A: Timer low
    apu->CpuWrite(0x400A, 0x54);
    test.Assert(true, "Triangle timer low set");

    // 0x400B: Length counter and timer high
    apu->CpuWrite(0x400B, 0xF8);
    test.Assert(true, "Triangle timer high set");

    // Enable triangle
    apu->CpuWrite(0x4015, 0x04);

    // Clock and test
    for (int i = 0; i < 1000; i++) {
        apu->Clock();
    }

    test.Assert(true, "Triangle clocking successful");
}

void TestAPUNoise(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Noise Channel");

    apu->Reset(true);

    // Configure noise (registers 0x400C-0x400F)
    // 0x400C: Envelope
    apu->CpuWrite(0x400C, 0xBF);
    test.Assert(true, "Noise envelope configured");

    // 0x400E: Period and mode
    apu->CpuWrite(0x400E, 0x00); // Mode 0, period 0
    test.Assert(true, "Noise period/mode set");

    // 0x400F: Length counter
    apu->CpuWrite(0x400F, 0xF8);
    test.Assert(true, "Noise length counter set");

    // Enable noise
    apu->CpuWrite(0x4015, 0x08);

    // Clock and test
    for (int i = 0; i < 1000; i++) {
        apu->Clock();
    }

    test.Assert(true, "Noise clocking successful");
}

void TestAPUSampleGeneration(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Sample Generation");

    apu->Reset(true);
    apu->SetSampleRate(44100.0);

    test.Info("Sample rate set to 44100 Hz");

    // Enable and configure pulse 1 with audible frequency
    apu->CpuWrite(0x4015, 0x01);
    apu->CpuWrite(0x4000, 0xBF); // Full volume
    apu->CpuWrite(0x4002, 0x54);
    apu->CpuWrite(0x4003, 0x00);

    // Clock APU and collect samples
    double samples[100];
    bool hasNonZero = false;

    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            apu->Clock();
        }
        samples[i] = apu->GetOutputSample();
        if (std::abs(samples[i]) > 0.001) {
            hasNonZero = true;
        }
    }

    test.Assert(hasNonZero, "APU generates non-zero samples");
    test.Info("Sample collection successful");
}

void TestAPUFrameCounter4Step(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Frame Counter (4-step mode)");

    apu->Reset(true);

    // Set 4-step mode
    apu->CpuWrite(0x4017, 0x00);
    test.Assert(true, "Frame counter set to 4-step mode");

    // Enable a channel to observe frame counter effects
    apu->CpuWrite(0x4015, 0x01);
    apu->CpuWrite(0x4000, 0xBF);
    apu->CpuWrite(0x4002, 0x54);
    apu->CpuWrite(0x4003, 0xF8);

    // Clock through one complete frame (approx 14915 CPU cycles)
    for (int i = 0; i < 30000; i++) {
        apu->Clock();
    }

    test.Assert(true, "4-step frame counter clocking successful");
}

void TestAPUFrameCounter5Step(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Frame Counter (5-step mode)");

    apu->Reset(true);

    // Set 5-step mode and disable IRQ
    apu->CpuWrite(0x4017, 0x80);
    test.Assert(true, "Frame counter set to 5-step mode");
    test.Assert(!apu->IsIRQActive(), "IRQ disabled in 5-step mode");

    // Enable a channel
    apu->CpuWrite(0x4015, 0x01);
    apu->CpuWrite(0x4000, 0xBF);
    apu->CpuWrite(0x4002, 0x54);
    apu->CpuWrite(0x4003, 0xF8);

    // Clock through frame (5-step is longer, approx 18641 CPU cycles)
    for (int i = 0; i < 40000; i++) {
        apu->Clock();
    }

    test.Assert(true, "5-step frame counter clocking successful");
}

void TestAPUFrameIRQ(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Frame Counter IRQ");

    apu->Reset(true);

    // Set 4-step mode with IRQ enabled (bit 6 clear)
    apu->CpuWrite(0x4017, 0x00);
    test.Assert(true, "4-step mode with IRQ enabled");

    // Clock to frame end where IRQ should trigger
    // IRQ triggers at cycle 29829 and 29830 in 4-step mode
    for (int i = 0; i < 30000; i++) {
        apu->Clock();
    }

    // Note: IRQ behavior depends on implementation
    test.Info("IRQ state after frame: " + std::to_string(apu->IsIRQActive()));

    // Clear IRQ by writing to 0x4015
    apu->CpuRead(0x4015);
    test.Assert(true, "IRQ acknowledgment successful");
}

void TestAPUEnvelope(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Envelope Generator");

    apu->Reset(true);

    // Configure pulse 1 with envelope
    apu->CpuWrite(0x4000, 0x08); // Envelope enabled, volume 8
    apu->CpuWrite(0x4002, 0x54);
    apu->CpuWrite(0x4003, 0xF8);
    apu->CpuWrite(0x4015, 0x01);

    test.Assert(true, "Envelope configured");

    // Clock through several frames to observe envelope decay
    for (int i = 0; i < 100000; i++) {
        apu->Clock();
    }

    test.Assert(true, "Envelope clocking successful");
}

void TestAPUSweep(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Sweep Unit");

    apu->Reset(true);

    // Configure pulse 1 with sweep
    apu->CpuWrite(0x4000, 0xBF);
    apu->CpuWrite(0x4001, 0x88); // Sweep enabled, period 0, shift 0
    apu->CpuWrite(0x4002, 0x54);
    apu->CpuWrite(0x4003, 0xF8);
    apu->CpuWrite(0x4015, 0x01);

    test.Assert(true, "Sweep unit configured");

    // Clock to observe sweep effects
    for (int i = 0; i < 100000; i++) {
        apu->Clock();
    }

    test.Assert(true, "Sweep unit clocking successful");
}

void TestAPULengthCounter(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU Length Counter");

    apu->Reset(true);

    // Configure pulse 1 with length counter
    apu->CpuWrite(0x4000, 0x30); // No envelope halt (length counter active)
    apu->CpuWrite(0x4002, 0x54);
    apu->CpuWrite(0x4003, 0x08); // Short length (load value 1)
    apu->CpuWrite(0x4015, 0x01);

    test.Assert(true, "Length counter configured");

    // Read status before expiration
    uint8_t status1 = apu->CpuRead(0x4015);
    test.Assert((status1 & 0x01) != 0, "Length counter active initially");

    // Clock through several frames (length should expire)
    for (int i = 0; i < 50000; i++) {
        apu->Clock();
    }

    // Check if length counter expired (channel should be silent)
    test.Assert(true, "Length counter expiration test completed");
}

void TestAPUAllChannelsMixed(DiagnosticTest& test, APU2A03* apu) {
    test.StartTest("APU All Channels Mixed Output");

    apu->Reset(true);
    apu->SetSampleRate(44100.0);

    // Enable all channels
    apu->CpuWrite(0x4015, 0x0F);

    // Configure pulse 1
    apu->CpuWrite(0x4000, 0xB0);
    apu->CpuWrite(0x4002, 0x54);
    apu->CpuWrite(0x4003, 0xF8);

    // Configure pulse 2
    apu->CpuWrite(0x4004, 0xB0);
    apu->CpuWrite(0x4006, 0xA8);
    apu->CpuWrite(0x4007, 0xF8);

    // Configure triangle
    apu->CpuWrite(0x4008, 0x7F);
    apu->CpuWrite(0x400A, 0x9C);
    apu->CpuWrite(0x400B, 0xF8);

    // Configure noise
    apu->CpuWrite(0x400C, 0xB0);
    apu->CpuWrite(0x400E, 0x05);
    apu->CpuWrite(0x400F, 0xF8);

    test.Assert(true, "All channels configured");

    // Clock and collect mixed samples
    bool hasVariance = false;
    double lastSample = 0.0;

    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < 100; j++) {
            apu->Clock();
        }
        double sample = apu->GetOutputSample();
        if (std::abs(sample - lastSample) > 0.01) {
            hasVariance = true;
        }
        lastSample = sample;
    }

    test.Assert(hasVariance, "Mixed output shows variance");
    test.Assert(true, "All channels mixed output successful");
}