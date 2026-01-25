#include "CPU6502.h"

// Enable/disable hardware-accurate dummy writes for RMW instructions
#define DUMMY_WRITES 1
#define DUMMY_READS 1

// ARITHMETIC INSTRUCTIONS
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

// LOGICAL INSTRUCTIONS
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

uint8_t CPU6502::BIT() {
    Fetch();
    _temp = A & _fetched;
    SetFlag(Z, (_temp & 0xFF) == 0);
    SetFlag(N, (_fetched & 0x80) != 0);
    SetFlag(V, (_fetched & 0x40) != 0);
    return 0;
}

// SHIFT AND ROTATE INSTRUCTIONS
uint8_t CPU6502::ASL() {
    Fetch();
#if DUMMY_WRITES
    if (_instructions[_opcode].modeType != AddrMode::IMP) {
        Write(_addrAbs, _fetched); // Dummy write of original value
    }
#endif
    _temp = static_cast<uint16_t>(_fetched) << 1;
    SetFlag(C, (_temp & 0xFF00) != 0);
    SetFlag(Z, (_temp & 0xFF) == 0);
    SetFlag(N, (_temp & 0x80) != 0);
    uint8_t result = _temp & 0xFF;
    if (_instructions[_opcode].modeType == AddrMode::IMP) {
        A = result;
    }
    else {
        Write(_addrAbs, result);
    }
    return 0;
}

uint8_t CPU6502::LSR() {
    Fetch();
#if DUMMY_WRITES
    if (_instructions[_opcode].modeType != AddrMode::IMP) {
        Write(_addrAbs, _fetched); // Dummy write of original value
    }
#endif
    SetFlag(C, (_fetched & 1) != 0);
    _temp = _fetched >> 1;
    SetFlag(Z, (_temp & 0xFF) == 0);
    SetFlag(N, (_temp & 0x80) != 0);
    if (_instructions[_opcode].modeType == AddrMode::IMP) {
        A = _temp & 0xFF;
    }
    else {
        Write(_addrAbs, _temp & 0xFF);
    }
    return 0;
}

uint8_t CPU6502::ROL() {
    Fetch();
#if DUMMY_WRITES
    if (_instructions[_opcode].modeType != AddrMode::IMP) {
        Write(_addrAbs, _fetched); // Dummy write of original value
    }
#endif
    _temp = (static_cast<uint16_t>(_fetched) << 1) | GetFlag(C);
    SetFlag(C, (_temp & 0x100) != 0);
    uint8_t result = _temp & 0xFF;
    SetFlag(Z, result == 0);
    SetFlag(N, (result & 0x80) != 0);
    if (_instructions[_opcode].modeType == AddrMode::IMP) {
        A = _temp & 0xFF;
    }
    else {
        Write(_addrAbs, _temp & 0xFF);
    }
    return 0;
}

uint8_t CPU6502::ROR() {
    Fetch();
#if DUMMY_WRITES
    if (_instructions[_opcode].modeType != AddrMode::IMP) {
        Write(_addrAbs, _fetched); // Dummy write of original value
    }
#endif
    _temp = (static_cast<uint16_t>(GetFlag(C)) << 7) | (_fetched >> 1);
    SetFlag(C, (_fetched & 1) != 0);
    SetFlag(Z, (_temp & 0xFF) == 0);
    SetFlag(N, (_temp & 0x80) != 0);
    if (_instructions[_opcode].modeType == AddrMode::IMP) {
        A = _temp & 0xFF;
    }
    else {
        Write(_addrAbs, _temp & 0xFF);
    }
    return 0;
}

// INCREMENT AND DECREMENT INSTRUCTIONS
uint8_t CPU6502::INC() {
    Fetch();
#if DUMMY_WRITES
    Write(_addrAbs, _fetched); // Dummy write of original value
#endif
    _temp = static_cast<uint16_t>((_fetched + 1) & 0xFF);
    Write(_addrAbs, _temp & 0xFF);
    SetFlag(Z, (_temp & 0xFF) == 0);
    SetFlag(N, (_temp & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::DEC() {
    Fetch();
#if DUMMY_WRITES
    Write(_addrAbs, _fetched); // Dummy write of original value
#endif
    _temp = static_cast<uint16_t>((_fetched - 1) & 0xFF);
    Write(_addrAbs, _temp & 0xFF);
    SetFlag(Z, (_temp & 0xFF) == 0);
    SetFlag(N, (_temp & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::INX() {
    X = (X + 1) & 0xFF;
    SetFlag(Z, X == 0);
    SetFlag(N, (X & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::DEX() {
    X = (X - 1) & 0xFF;
    SetFlag(Z, X == 0);
    SetFlag(N, (X & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::INY() {
    Y = (Y + 1) & 0xFF;
    SetFlag(Z, Y == 0);
    SetFlag(N, (Y & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::DEY() {
    Y = (Y - 1) & 0xFF;
    SetFlag(Z, Y == 0);
    SetFlag(N, (Y & 0x80) != 0);
    return 0;
}

// COMPARE INSTRUCTIONS
uint8_t CPU6502::CMP() {
    Fetch();
    _temp = static_cast<uint16_t>((A - _fetched) & 0xFF);
    SetFlag(C, A >= _fetched);
    SetFlag(Z, _temp == 0);
    SetFlag(N, (_temp & 0x80) != 0);
    return 1;
}

uint8_t CPU6502::CPX() {
    Fetch();
    _temp = static_cast<uint16_t>((X - _fetched) & 0xFF);
    SetFlag(C, X >= _fetched);
    SetFlag(Z, _temp == 0);
    SetFlag(N, (_temp & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::CPY() {
    Fetch();
    _temp = static_cast<uint16_t>((Y - _fetched) & 0xFF);
    SetFlag(C, Y >= _fetched);
    SetFlag(Z, _temp == 0);
    SetFlag(N, (_temp & 0x80) != 0);
    return 0;
}

// LOAD AND STORE INSTRUCTIONS
uint8_t CPU6502::LDA() {
    Fetch();
    A = _fetched;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 1;
}

uint8_t CPU6502::LDX() {
    Fetch();
    X = _fetched;
    SetFlag(Z, X == 0);
    SetFlag(N, (X & 0x80) != 0);
    return 1;
}

uint8_t CPU6502::LDY() {
    Fetch();
    Y = _fetched;
    SetFlag(Z, Y == 0);
    SetFlag(N, (Y & 0x80) != 0);
    return 1;
}

uint8_t CPU6502::STA() {
//#if DUMMY_READS
//    // CYCLE-ACCURATE: Indexed store instructions ALWAYS perform a dummy read
//    // at the incorrectly calculated address (before carry propagation) on cycle 4,
//    // regardless of whether a page boundary was crossed.
//    // This is critical for passing test ROMs like instr_test-v5
//    AddrMode mode = _instructions[_opcode].modeType;
//    if (mode == AddrMode::ABX || mode == AddrMode::ABY) {
//        // For absolute indexed: dummy read at (base_high : (base_low + index) & 0xFF)
//        uint8_t index = (mode == AddrMode::ABX) ? X : Y;
//        uint16_t dummyAddr = (_addrAbs_Base & 0xFF00) | ((_addrAbs_Base + index) & 0x00FF);
//        Read(dummyAddr);
//    }
//    else if (mode == AddrMode::IZY) {
//        // For indirect indexed: dummy read at (pointer_high : (pointer_low + Y) & 0xFF)
//        uint16_t dummyAddr = (_addrAbs_Base & 0xFF00) | ((_addrAbs_Base + Y) & 0x00FF);
//        Read(dummyAddr);
//    }
//#endif
    Write(_addrAbs, A);
    return 0;
}

uint8_t CPU6502::STX() {
    Write(_addrAbs, X);
    return 0;
}

uint8_t CPU6502::STY() {
    Write(_addrAbs, Y);
    return 0;
}

// TRANSFER INSTRUCTIONS
uint8_t CPU6502::TAX() {
    X = A;
    SetFlag(Z, X == 0);
    SetFlag(N, (X & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::TAY() {
    Y = A;
    SetFlag(Z, Y == 0);
    SetFlag(N, (Y & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::TXA() {
    A = X;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::TYA() {
    A = Y;
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::TSX() {
    X = SP;
    SetFlag(Z, X == 0);
    SetFlag(N, (X & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::TXS() {
    SP = X;
    return 0;
}

// STACK INSTRUCTIONS
uint8_t CPU6502::PHA() {
    Push(A);
    return 0;
}

uint8_t CPU6502::PLA() {
    A = Pop();
    SetFlag(Z, A == 0);
    SetFlag(N, (A & 0x80) != 0);
    return 0;
}

uint8_t CPU6502::PHP() {
    Push(Status | B | U);
    SetFlag(B, false);
    SetFlag(U, false);
    return 0;
}

uint8_t CPU6502::PLP() {
    Status = (Pop() & 0xEF) | 0x20; // B does not transfer to PLP
    return 0;
}

// BRANCH INSTRUCTIONS
uint8_t CPU6502::BCC() {
    return Branch(GetFlag(C) == 0);
}

uint8_t CPU6502::BCS() {
    return Branch(GetFlag(C) == 1);
}

uint8_t CPU6502::BEQ() {
    return Branch(GetFlag(Z) == 1);
}

uint8_t CPU6502::BMI() {
    return Branch(GetFlag(N) == 1);
}

uint8_t CPU6502::BNE() {
    return Branch(GetFlag(Z) == 0);
}

uint8_t CPU6502::BPL() {
    return Branch(GetFlag(N) == 0);
}

uint8_t CPU6502::BVC() {
    return Branch(GetFlag(V) == 0);
}

uint8_t CPU6502::BVS() {
    return Branch(GetFlag(V) == 1);
}

// JUMP AND CALL INSTRUCTIONS
uint8_t CPU6502::JMP() {
    PC = _addrAbs;
    return 0;
}

uint8_t CPU6502::JSR() {
    PC--;
    PushWord(PC);
    PC = _addrAbs;
    return 0;
}

uint8_t CPU6502::RTS() {
    PC = PopWord();
    PC++;
    return 0;
}

uint8_t CPU6502::RTI() {
    Status = Pop();
    Status &= ~B;
    Status &= ~U;
    PC = PopWord();
    return 0;
}

uint8_t CPU6502::BRK() {
    // all_instrs.nes fixed
    // BRK pushes PC+2, then jumps to IRQ/BRK vector
    // PC has already been incremented by IMM addressing mode
    PushWord(PC);

    // Push status with B and U flags set
    Push(Status | B | U);

    // Set interrupt disable flag
    SetFlag(I, true);

    // Read IRQ/BRK vector at $FFFE-$FFFF
    uint8_t lo = Read(0xFFFE);
    uint8_t hi = Read(0xFFFF);
    PC = (static_cast<uint16_t>(hi) << 8) | lo;

    return 0;
}

// FLAG INSTRUCTIONS
uint8_t CPU6502::CLC() {
    SetFlag(C, false);
    return 0;
}

uint8_t CPU6502::CLD() {
    SetFlag(D, false);
    return 0;
}

uint8_t CPU6502::CLI() {
    SetFlag(I, false);
    return 0;
}

uint8_t CPU6502::CLV() {
    SetFlag(V, false);
    return 0;
}

uint8_t CPU6502::SEC() {
    SetFlag(C, true);
    return 0;
}

uint8_t CPU6502::SED() {
    SetFlag(D, true);
    return 0;
}

uint8_t CPU6502::SEI() {
    SetFlag(I, true);
    return 0;
}

uint8_t CPU6502::NOP() {
    // Some NOPs take an extra cycle on page boundary
    switch (_opcode) {
    case 0x1C: case 0x3C: case 0x5C: case 0x7C:
    case 0xDC: case 0xFC:
        return 1;
    }
    return 0;
}