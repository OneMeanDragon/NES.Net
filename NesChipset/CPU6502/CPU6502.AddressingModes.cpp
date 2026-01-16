#include "CPU6502.h"

// Note: Addressing modes are already implemented in CPU6502.Core.cpp
// This file is kept for organizational purposes matching the VB.NET structure

// The following addressing modes are implemented in CPU6502.Core.cpp:
// - IMP() - Implied
// - IMM() - Immediate
// - ZP0() - Zero Page
// - ZPX() - Zero Page, X
// - ZPY() - Zero Page, Y
// - REL() - Relative (for branches)
// - ABS() - Absolute
// - ABX() - Absolute, X
// - ABY() - Absolute, Y
// - IND() - Indirect (JMP only)
// - IZX() - Indexed Indirect (Zero Page, X)
// - IZY() - Indirect Indexed (Zero Page), Y

uint8_t CPU6502::IMP() {
    _fetched = A;
    return 0;
}

uint8_t CPU6502::IMM() {
    _addrAbs = PC++;
    return 0;
}

uint8_t CPU6502::ZP0() {
    _addrAbs = Read(PC++);
    _addrAbs &= 0x00FF;
    return 0;
}

uint8_t CPU6502::ZPX() {
    uint8_t base = Read(PC++);
    _addrAbs = (base + X) & 0xFF;
    return 0;
}

uint8_t CPU6502::ZPY() {
    uint8_t base = Read(PC++);
    _addrAbs = (base + Y) & 0xFF;
    return 0;
}

uint8_t CPU6502::REL() {
    _addrRel = Read(PC++);
    if (_addrRel & 0x80) {
        _addrRel |= 0xFF00;
    }
    return 0;
}

uint8_t CPU6502::ABS() {
    uint8_t lo = Read(PC++);
    uint8_t hi = Read(PC++);
    _addrAbs = (static_cast<uint16_t>(hi) << 8) | lo;
    _addrAbs_Base = _addrAbs;
    return 0;
}

// all_instrs.nes fixed
uint8_t CPU6502::ABX() {
    ABS();
    uint16_t baseAddr = _addrAbs;
    _addrAbs = (_addrAbs + X) & 0xFFFF;

    // Page boundary crossed - perform dummy read
    if ((_addrAbs & 0xFF00) != (baseAddr & 0xFF00)) {
#if DUMMY_READS
        // Dummy read at incorrectly calculated address
        uint16_t dummyAddr = (baseAddr & 0xFF00) | (_addrAbs & 0x00FF);
        Read(dummyAddr);
#endif
        if (_opcode == 0x9C/*SHY*/ || _opcode == 0x9D/*STA*/) return 0;
        return 1; // Signal extra cycle needed
    }
    return 0;
}

// all_instrs.nes fixed
uint8_t CPU6502::ABY() {
    ABS();
    uint16_t baseAddr = _addrAbs;
    _addrAbs = (_addrAbs + Y) & 0xFFFF;

    // Page boundary crossed - perform dummy read
    if ((_addrAbs & 0xFF00) != (baseAddr & 0xFF00)) {
#if DUMMY_READS
        // Dummy read at incorrectly calculated address
        uint16_t dummyAddr = (baseAddr & 0xFF00) | (_addrAbs & 0x00FF);
        Read(dummyAddr);
#endif
        if (_opcode == 0x9E/*SHX*/ || _opcode == 0x99/*STA*/) return 0;
        return 1; // Signal extra cycle needed
    }
    return 0;
}

uint8_t CPU6502::IND() {
    ABS(); // Fetch pointer address
    uint8_t lo = Read(_addrAbs);

    // Hardware bug: if pointer is at page boundary (xxFF), wrap within page
    uint16_t hiAddr;
    if ((_addrAbs & 0x00FF) == 0x00FF) {
        hiAddr = _addrAbs & 0xFF00; // Wrap to xx00
    }
    else {
        hiAddr = _addrAbs + 1;
    }

    uint8_t hi = Read(hiAddr);
    _addrAbs = (static_cast<uint16_t>(hi) << 8) | lo;
    return 0;
}

uint8_t CPU6502::IZX() {
    uint8_t t = Read(PC++);
    uint8_t lo = Read((t + X) & 0xFF);
    uint8_t hi = Read((t + X + 1) & 0xFF);
    _addrAbs = (static_cast<uint16_t>(hi) << 8) | lo;
    return 0;
}

uint8_t CPU6502::IZY() {
    uint8_t t = Read(PC++);
    uint8_t lo = Read(t & 0xFF);
    uint8_t hi = Read((t + 1) & 0xFF);

    _addrAbs_Base = (static_cast<uint16_t>(hi) << 8) | lo;
    _addrAbs = (_addrAbs_Base + Y) & 0xFFFF;

    // Page boundary crossed - perform dummy read
    if ((_addrAbs & 0xFF00) != (_addrAbs_Base & 0xFF00)) {
#if DUMMY_READS
        // Dummy read at incorrectly calculated address
        uint16_t dummyAddr = (_addrAbs_Base & 0xFF00) | (_addrAbs & 0x00FF);
        Read(dummyAddr);
#endif
        return 1; // Signal extra cycle needed
    }
    return 0;
}