#include "CPU6502.h"

// ILLEGAL/UNDOCUMENTED INSTRUCTIONS
// These include proper dummy read/write cycles for accuracy

#define DUMMY_WRITES 1
#define DUMMY_READS 1

uint8_t CPU6502::KIL() {
    PC--;
    // CPU halted - would need external reset
    return 0;
}

uint8_t CPU6502::LAX() {
    Fetch();
    A = _fetched;
    X = _fetched;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 1;
}

uint8_t CPU6502::SAX() {
    Write(_addrAbs, A & X);
    return 0;
}

uint8_t CPU6502::DCP() {
    // DCP is DEC + CMP (Read-Modify-Write)
    uint8_t data = Read(_addrAbs);
#if DUMMY_WRITES
    Write(_addrAbs, data); // Dummy write
#endif
    data = (data - 1) & 0xFF;
    Write(_addrAbs, data);

    // CMP logic
    SetFlag(C, A >= data);
    uint8_t temp = (A - data) & 0xFF;
    SetFlag(Z, temp == 0);
    SetFlag(N, (temp & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::ISB() {
    // ISB is INC + SBC (Read-Modify-Write)
    uint8_t data = Read(_addrAbs);
#if DUMMY_WRITES
    Write(_addrAbs, data); // Dummy write
#endif
    data = (data + 1) & 0xFF;
    Write(_addrAbs, data);

    // SBC logic
    uint8_t value = data ^ 0xFF;
    _temp = static_cast<uint16_t>(A) + value + GetFlag(C);
    SetFlag(V, ((_temp ^ A) & (_temp ^ value) & 0x80) != 0);
    SetFlag(C, _temp > 0xFF);
    A = _temp & 0xFF;
    SetFlag(N, (A & 0x80) != 0);
    SetFlag(Z, A == 0);
    return 0;
}

uint8_t CPU6502::SLO() {
    // SLO is ASL + ORA (Read-Modify-Write)
    uint8_t data = Read(_addrAbs);
#if DUMMY_WRITES
    Write(_addrAbs, data); // Dummy write
#endif
    SetFlag(C, (data & 0x80) != 0);
    data = (static_cast<uint16_t>(data) << 1) & 0xFF;
    Write(_addrAbs, data);

    // ORA logic
    A = A | data;
    SetFlag(N, (A & 0x80) != 0);
    SetFlag(Z, A == 0);
    return 0;
}

uint8_t CPU6502::RLA() {
    // RLA is ROL + AND (Read-Modify-Write)
    uint8_t data = Read(_addrAbs);
#if DUMMY_WRITES
    Write(_addrAbs, data); // Dummy write
#endif
    uint8_t bit7 = (data & 0x80) ? 1 : 0;
    data = ((static_cast<uint16_t>(data) << 1) | GetFlag(C)) & 0xFF;
    SetFlag(C, bit7 == 1);
    Write(_addrAbs, data);

    // AND logic
    A = A & data;
    SetFlag(N, (A & 0x80) != 0);
    SetFlag(Z, A == 0);
    return 0;
}

uint8_t CPU6502::SRE() {
    // SRE is LSR + EOR (Read-Modify-Write)
    uint8_t data = Read(_addrAbs);
#if DUMMY_WRITES
    Write(_addrAbs, data); // Dummy write
#endif
    SetFlag(C, (data & 1) != 0);
    data = data >> 1;
    Write(_addrAbs, data);

    // EOR logic
    A = A ^ data;
    SetFlag(N, (A & 0x80) != 0);
    SetFlag(Z, A == 0);
    return 0;
}

uint8_t CPU6502::RRA() {
    // RRA is ROR + ADC (Read-Modify-Write)
    uint8_t data = Read(_addrAbs);
#if DUMMY_WRITES
    Write(_addrAbs, data); // Dummy write
#endif
    uint8_t bit0 = (data & 1) ? 1 : 0;
    data = (data >> 1) | (GetFlag(C) << 7);
    SetFlag(C, bit0 == 1);
    Write(_addrAbs, data);

    // ADC logic
    _temp = static_cast<uint16_t>(A) + data + GetFlag(C);
    SetFlag(V, ((static_cast<uint16_t>(A) ^ _temp) & (static_cast<uint16_t>(data) ^ _temp) & 0x80) != 0);
    SetFlag(C, _temp > 0xFF);
    A = _temp & 0xFF;
    SetFlag(N, (A & 0x80) != 0);
    SetFlag(Z, A == 0);
    return 0;
}

uint8_t CPU6502::ANC() {
    uint8_t data = Read(_addrAbs);
    A = A & data;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    SetFlag(C, GetFlag(N));
    return 0;
}

uint8_t CPU6502::ALR() {
    uint8_t data = Read(_addrAbs);
    A = A & data;
    SetFlag(C, (A & 1) != 0);
    A = A >> 1;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::ARR() {
    uint8_t data = Read(_addrAbs);
    uint8_t result = A & data;
    A = (result >> 1) | (GetFlag(C) << 7);
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    SetFlag(C, (A & 0x40) != 0);

    // Overflow flag special logic
    uint8_t bit6 = (A >> 6) & 1;
    uint8_t bit5 = (A >> 5) & 1;
    SetFlag(V, (bit6 ^ bit5) != 0);
    return 0;
}

uint8_t CPU6502::AXS() {
    uint8_t data = Read(_addrAbs);
    uint8_t combined = A & X;
    uint8_t result = combined - data;
    SetFlag(C, combined >= data);
    X = result & 0xFF;
    SetFlag(Z, X == 0);
    SetFlag(N, (X & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::XAA() {
    uint8_t data = Read(_addrAbs);
    // XAA is highly unstable - magic constant varies by chip
    uint8_t magic = 0xFF;
    A = (A | magic) & X & data;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::LAS() {
    Fetch();
    uint8_t result = _fetched & SP;
    A = result;
    X = result;
    SP = result;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 1;
}

uint8_t CPU6502::SHA() {
    // SHA stores A & X & (high_byte + 1)
    // NOTE: On page boundary crossing, the high byte used is unstable
    uint8_t highBytePlus1 = ((_addrAbs >> 8) & 0xFF) + 1;
    uint8_t result = A & X & highBytePlus1;
    Write(_addrAbs, result);
    return 0;
}

// all_instrs.nes fixed
uint8_t CPU6502::SHX() {
    // SHX stores X & (high_byte + 1)
    // NOTE: On page boundary crossing, the address itself may be corrupted
    uint16_t base = _addrAbs - Y; // Recover original operand high byte
    uint16_t hi = (base >> 8);
    uint8_t res = X & (hi + 1);

    // QUIRK: High byte of address is replaced by the result if page crossed
    if ((_addrAbs & 0xFF00) != (base & 0xFF00)) {
        _addrAbs = (static_cast<uint16_t>(res) << 8) | (_addrAbs & 0x00FF);
    }

    Write(_addrAbs, res);
    return 0;
}

// all_instrs.nes fixed
uint8_t CPU6502::SHY() {
    // SHY stores Y & (high_byte + 1)
    // NOTE: On page boundary crossing, the address itself may be corrupted
    uint16_t base = _addrAbs - X; // Recover original operand high byte
    uint16_t hi = (base >> 8);
    uint8_t res = Y & (hi + 1);

    // QUIRK: If a page was crossed, the target address high byte 
    // becomes the value intended to be stored.
    if ((_addrAbs & 0xFF00) != (base & 0xFF00)) {
        _addrAbs = (static_cast<uint16_t>(res) << 8) | (_addrAbs & 0x00FF);
    }

    Write(_addrAbs, res);
    return 0;
}

uint8_t CPU6502::TAS() {
    // TAS sets SP = A & X, then stores SP & (high_byte + 1)
    SP = A & X;
    uint8_t targetHighByte = (_addrAbs >> 8) & 0xFF;
    uint8_t result = SP & (targetHighByte + 1);
    Write(_addrAbs, result);
    return 0;
}

uint8_t CPU6502::ATX() {
    //uint8_t data = Read(_addrAbs);
    //A = data;
    //X = A;
    //SetFlag(N, (A & 0x80) != 0);
    //SetFlag(Z, A == 0);
    //return 0;
    // 
    // ATX (0xAB) - Also known as LXA/OAL
    // This is HIGHLY unstable across different chip revisions
    // For maximum compatibility, we use the most common stable behavior:
    // A = X = (A | CONST) & immediate
    // Where CONST is typically $EE, $FF, or $00 depending on chip
    // Using $FF (most common) for better test ROM compatibility
    uint8_t data = Read(_addrAbs);
    uint8_t magic = 0xFF; // Magic constant - varies by chip revision
    A = (A | magic) & data;
    X = A;
    SetFlag(N, (A & 0x80) != 0);
    SetFlag(Z, A == 0);
    return 0;
}