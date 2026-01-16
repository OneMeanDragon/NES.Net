#include "CPU6502.h"
#include "../NESBus.h"

// Enable/disable hardware-accurate dummy reads/writes
#define DUMMY_READS 1
#define DUMMY_WRITES 1

CPU6502::CPU6502()
    : A(0), X(0), Y(0), SP(0), PC(0), Status(0),
    ClockCount(0), InstructionCount(0),
    _fetched(0), _addrAbs(0), _addrAbs_Base(0), _addrRel(0),
    _opcode(0), _cycles(0), _temp(0), _bus(nullptr)
{
    InitializeInstructionTable();
}

CPU6502::~CPU6502() {
}

void CPU6502::ConnectBus(NESBus* bus) {
    _bus = bus;
}

uint8_t CPU6502::Read(uint16_t addr) {
    return _bus->CpuRead(addr, false);
}

void CPU6502::Write(uint16_t addr, uint8_t data) {
    _bus->CpuWrite(addr, data);
}

// Flag operations
uint8_t CPU6502::GetFlag(StatusFlags flag) {
    return (Status & flag) ? 1 : 0;
}

void CPU6502::SetFlag(StatusFlags flag, bool value) {
    if (value) {
        Status |= flag;
    }
    else {
        Status &= ~flag;
    }
}

// Stack operations
void CPU6502::Push(uint8_t data) {
    Write(0x0100 + SP, data);
    SP = (SP - 1) & 0xFF;
}

uint8_t CPU6502::Pop() {
    SP = (SP + 1) & 0xFF;
    return Read(0x0100 + SP);
}

void CPU6502::PushWord(uint16_t data) {
    Push((data >> 8) & 0xFF);
    Push(data & 0xFF);
}

uint16_t CPU6502::PopWord() {
    uint8_t lo = Pop();
    uint8_t hi = Pop();
    return (static_cast<uint16_t>(hi) << 8) | lo;
}

// Fetch helper
uint8_t CPU6502::Fetch() {
    if (_instructions[_opcode].modeType != AddrMode::IMP) {
        _fetched = Read(_addrAbs);
    }
    return _fetched;
}

// Reset
void CPU6502::Reset() {
    _addrAbs = 0xFFFC;
    uint8_t lo = Read(_addrAbs);
    uint8_t hi = Read(_addrAbs + 1);
    PC = (static_cast<uint16_t>(hi) << 8) | lo;

    A = 0;
    X = 0;
    Y = 0;
    SP = 0xFD;
    Status = U;

    _addrRel = 0;
    _addrAbs = 0;
    _fetched = 0;

    _cycles = 8;
}

// IRQ
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

// NMI
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

// Clock
void CPU6502::Clock() {
    if (_cycles == 0) {
        _opcode = Read(PC);
        SetFlag(U, true);
        PC++;

        _cycles = _instructions[_opcode].cycles;

        uint8_t additionalCycle1 = (this->*_instructions[_opcode].addressMode)();
        uint8_t additionalCycle2 = (this->*_instructions[_opcode].operate)();

        _cycles += (additionalCycle1 & additionalCycle2);

        SetFlag(U, true);
        InstructionCount++;
    }

    ClockCount++;
    _cycles--;
}

// ADDRESSING MODES
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

uint8_t CPU6502::ABX() {
    ABS();
    uint16_t baseAddr = _addrAbs;
    _addrAbs = (_addrAbs + X) & 0xFFFF;

    if ((_addrAbs & 0xFF00) != (baseAddr & 0xFF00)) {
#if DUMMY_READS
        uint16_t dummyAddr = (baseAddr & 0xFF00) | (_addrAbs & 0x00FF);
        Read(dummyAddr);
#endif
        return 1;
    }
    return 0;
}

uint8_t CPU6502::ABY() {
    ABS();
    uint16_t baseAddr = _addrAbs;
    _addrAbs = (_addrAbs + Y) & 0xFFFF;

    if ((_addrAbs & 0xFF00) != (baseAddr & 0xFF00)) {
        return 1;
    }
    return 0;
}

uint8_t CPU6502::IND() {
    ABS();
    uint8_t lo = Read(_addrAbs);

    uint16_t hiAddr;
    if ((_addrAbs & 0x00FF) == 0x00FF) {
        hiAddr = _addrAbs & 0xFF00;
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

    if ((_addrAbs & 0xFF00) != (_addrAbs_Base & 0xFF00)) {
#if DUMMY_READS
        uint16_t dummyAddr = (_addrAbs_Base & 0xFF00) | (_addrAbs & 0x00FF);
        Read(dummyAddr);
#endif
        return 1;
    }
    return 0;
}

// Branch helper
uint8_t CPU6502::Branch(bool condition) {
    if (condition) {
        _cycles++;
        _addrAbs = PC + _addrRel;

        if ((_addrAbs & 0xFF00) != (PC & 0xFF00)) {
            _cycles++;
        }
        PC = _addrAbs;
    }
    return 0;
}
