#include "../APU2A03.h"

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