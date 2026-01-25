#include "CPU6502.h"

// Note: Reset, IRQ, and NMI are already implemented in CPU6502.Core.cpp
// This file is kept for organizational purposes matching the VB.NET structure

// The following interrupt handling is implemented in CPU6502.Core.cpp:
// - Reset() - Reset the CPU to initial state (reads from $FFFC-$FFFD)
// - IRQ()   - Maskable interrupt request (reads from $FFFE-$FFFF)
// - NMI()   - Non-maskable interrupt (reads from $FFFA-$FFFB)

// IRQ - Maskable interrupt (7 cycles)
void CPU6502::IRQ() {
    if (GetFlag(I) == 0) {
        PushWord(PC);

        SetFlag(B, false);
        SetFlag(U, true);
        SetFlag(I, true);
        Push(Status);

        _addrAbs = 0xFFFE;
        uint8_t lo = Read(_addrAbs);
        uint8_t hi = Read(_addrAbs + 1);
        PC = (static_cast<uint16_t>(hi) << 8) | lo;

        _cycles = 7;
    }
}

// NMI - Non-maskable interrupt (8 cycles)
void CPU6502::NMI() {
    PushWord(PC);

    SetFlag(B, false);
    SetFlag(U, true);
    SetFlag(I, true);
    Push(Status);

    _addrAbs = 0xFFFA;
    uint8_t lo = Read(_addrAbs);
    uint8_t hi = Read(_addrAbs + 1);
    PC = (static_cast<uint16_t>(hi) << 8) | lo;

    _cycles = 8;
}
