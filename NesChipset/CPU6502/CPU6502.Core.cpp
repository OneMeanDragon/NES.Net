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
void CPU6502::Reset(bool coldstart) {
    if (!coldstart) { // we havent turned on the power yet, cold start the cpu
        PC = (static_cast<uint16_t>(Read(0xFFFD)) << 8) | Read(0xFFFC); // (hi << 8) | lo
        A = 0;
        X = 0;
        Y = 0;
        SP = 0xFD;
        Status = U | I; // 0x34
    }
    else {
        PC = (static_cast<uint16_t>(Read(0xFFFD)) << 8) | Read(0xFFFC); // (hi << 8) | lo
        if (SP >= 3) {
            SP -= 3;
        }
        else {
            SP = 0xFF - (3 - SP - 1);  // Wrap around
        }
        Status = (Status & ~I) | I; // 0x34
    }
    _addrRel = 0;
    _addrAbs = 0;
    _fetched = 0;

    _cycles = 8;
}

// Clock - Execute one CPU cycle
void CPU6502::Clock() {
    if (_cycles == 0) {
        // Fetch opcode
        _opcode = Read(PC);
        SetFlag(U, true);
        PC++;

        // Get base cycle count
        _cycles = _instructions[_opcode].cycles;

        // Execute addressing mode and instruction
        uint8_t additionalCycle1 = (this->*_instructions[_opcode].addressMode)();
        uint8_t additionalCycle2 = (this->*_instructions[_opcode].operate)();

        // Add extra cycles (only if both return 1)
        _cycles += (additionalCycle1 & additionalCycle2);

        SetFlag(U, true);
        InstructionCount++;
    }

    ClockCount++;
    _cycles--;
}

// Branch helper
uint8_t CPU6502::Branch(bool condition) {
    if (condition) {
        _cycles++; // Branch taken adds 1 cycle
        _addrAbs = PC + _addrRel;

        // Page boundary crossed adds another cycle
        if ((_addrAbs & 0xFF00) != (PC & 0xFF00)) {
            _cycles++;
        }
        PC = _addrAbs;
    }
    return 0;
}
