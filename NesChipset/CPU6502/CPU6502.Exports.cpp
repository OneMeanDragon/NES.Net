#include "CPU6502.h"

// DLL Export Functions for P/Invoke

// Lifecycle
DLLEXPORT CPU6502* CreateCPU() {
    return new CPU6502();
}

DLLEXPORT void DestroyCPU(CPU6502* cpu) {
    if (cpu) {
        delete cpu;
    }
}

// Core functions
DLLEXPORT void CPU_Reset(CPU6502* cpu, bool coldstart) {
    if (cpu) {
        cpu->Reset(coldstart);
    }
}

DLLEXPORT void CPU_Clock(CPU6502* cpu) {
    if (cpu) {
        cpu->Clock();
    }
}

// Interrupts
DLLEXPORT void CPU_IRQ(CPU6502* cpu) {
    if (cpu) {
        cpu->IRQ();
    }
}

DLLEXPORT void CPU_NMI(CPU6502* cpu) {
    if (cpu) {
        cpu->NMI();
    }
}

// Bus connection
DLLEXPORT void CPU_ConnectBus(CPU6502* cpu, NESBus* bus) {
    if (cpu) {
        cpu->ConnectBus(bus);
    }
}

// Status
DLLEXPORT bool CPU_IsComplete(CPU6502* cpu) {
    if (cpu) {
        return cpu->IsComplete();
    }
    return true;
}

// Debug/Inspection
DLLEXPORT uint16_t CPU_GetPC(CPU6502* cpu) {
    if (cpu) {
        return cpu->GetPC();
    }
    return 0;
}

DLLEXPORT uint8_t CPU_GetA(CPU6502* cpu) {
    if (cpu) {
        return cpu->GetA();
    }
    return 0;
}

DLLEXPORT uint8_t CPU_GetX(CPU6502* cpu) {
    if (cpu) {
        return cpu->GetX();
    }
    return 0;
}

DLLEXPORT uint8_t CPU_GetY(CPU6502* cpu) {
    if (cpu) {
        return cpu->GetY();
    }
    return 0;
}

DLLEXPORT uint8_t CPU_GetSP(CPU6502* cpu) {
    if (cpu) {
        return cpu->GetSP();
    }
    return 0;
}

DLLEXPORT uint8_t CPU_GetStatus(CPU6502* cpu) {
    if (cpu) {
        return cpu->GetStatus();
    }
    return 0;
}

DLLEXPORT uint64_t CPU_GetClockCount(CPU6502* cpu) {
    if (cpu) {
        return cpu->GetClockCount();
    }
    return 0;
}

// Aliases for Bus compatibility
DLLEXPORT void ClockCPU(CPU6502* cpu) {
    CPU_Clock(cpu);
}

DLLEXPORT void ResetCPU(CPU6502* cpu, bool coldstart) {
    CPU_Reset(cpu, coldstart);
}

DLLEXPORT void TriggerNMI(CPU6502* cpu) {
    CPU_NMI(cpu);
}

DLLEXPORT void TriggerIRQ(CPU6502* cpu) {
    CPU_IRQ(cpu);
}