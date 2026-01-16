#include "CPU6502.h"

// OFFICIAL INSTRUCTIONS
uint8_t CPU6502::ADC() {
    Fetch();
    _temp = static_cast<uint16_t>(A) + _fetched + GetFlag(C);
    SetFlag(C, _temp > 255);
    SetFlag(Z, (_temp & 0xFF) == 0);
    SetFlag(V, ((~(A ^ _fetched) & (A ^ _temp)) & 0x80) != 0);
    SetFlag(N, (_temp & 0x80) != 0);
    A = _temp & 0xFF;
    return 1;
}

uint8_t CPU6502::SBC() {
    Fetch();
    uint8_t value = _fetched ^ 0xFF;
    _temp = static_cast<uint16_t>(A) + value + GetFlag(C);
    SetFlag(C, (_temp & 0xFF00) != 0);
    SetFlag(Z, (_temp & 0xFF) == 0);
    SetFlag(V, ((_temp ^ A) & (_temp ^ value) & 0x80) != 0);
    SetFlag(N, (_temp & 0x80) != 0);
    A = _temp & 0xFF;
    return 1;
}

uint8_t CPU6502::AND() {
    Fetch();
    A = A & _fetched;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 1;
}

uint8_t CPU6502::ORA() {
    Fetch();
    A = A | _fetched;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 1;
}

uint8_t CPU6502::EOR() {
    Fetch();
    A = A ^ _fetched;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 1;
}
