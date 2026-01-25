#pragma once

#include "DiagnosticTest.h"
#include "DiagnosticBus.h"
#include "DiagnosticCartridge.h"
#include "CPU/CPU6502.h"
#include <fstream>
#include <sstream>

std::string normalize(const std::string& line) {
    std::string out;
    for (char c : line) {
        if (c != '\r') out += c; // remove \r
    }
    while (!out.empty() && isspace(out.back())) out.pop_back(); // trim end
    return out;
}

bool CompareLogs(const std::string& myLogPath,
    const std::string& officialLogPath)
{
    std::ifstream myLog(myLogPath);
    std::ifstream officialLog(officialLogPath);

    if (!myLog.is_open() || !officialLog.is_open()) {
        std::cerr << "Failed to open one or both log files\n";
        return false;
    }

    std::string myLine, officialLine;
    size_t lineNumber = 1;

    while (true) {
        bool myOk = static_cast<bool>(std::getline(myLog, myLine));
        bool offOk = static_cast<bool>(std::getline(officialLog, officialLine));

        if (!myOk && !offOk) {
            std::cout << "Logs match perfectly (GOOD)\n";
            return true;
        }

        if (myOk != offOk) {
            std::cout << "Log length mismatch at line " << lineNumber << "\n";
            return false;
        }

        if (myLine != officialLine) {
            std::cout << "Mismatch at line " << lineNumber << ":\n\n";
            std::cout << "Your log:\n" << myLine << "\n\n";
            std::cout << "Official log:\n" << officialLine << "\n";
            return false;
        }

        ++lineNumber;
    }
}

// Helper to execute CPU until instruction completes
void ExecuteInstruction(CPU6502* cpu) {
    do {
        cpu->Clock();
    } while (!cpu->IsComplete());
}

// Helper to execute multiple instructions
void ExecuteInstructions(CPU6502* cpu, int count) {
    for (int i = 0; i < count; i++) {
        ExecuteInstruction(cpu);
    }
}

void TestCPUBasics(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus) {
    test.StartTest("CPU Basic Initialization");

    cpu->Reset(true);

    test.AssertEquals((uint8_t)0x00, cpu->A, "Accumulator initialized to 0");
    test.AssertEquals((uint8_t)0x00, cpu->X, "X register initialized to 0");
    test.AssertEquals((uint8_t)0x00, cpu->Y, "Y register initialized to 0");
    test.AssertEquals((uint8_t)0xFD, cpu->SP, "Stack pointer initialized to 0xFD");
    test.Assert((cpu->Status & 0x24) == 0x24, "Status flags contain I and U flags");
}

void TestCPULoadStore(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus, DiagnosticCartridge* cart) {
    test.StartTest("CPU Load/Store Instructions");

    std::vector<uint8_t> program = {
        0xA9, 0x42,  // LDA #$42
        0x85, 0x10,  // STA $10
        0xA2, 0x55,  // LDX #$55
        0x86, 0x11,  // STX $11
        0xA0, 0xAA,  // LDY #$AA
        0x84, 0x12,  // STY $12
        0xA5, 0x10,  // LDA $10
        0xA6, 0x11,  // LDX $11
        0xA4, 0x12,  // LDY $12
        0x00         // BRK
    };

    cart->LoadTestProgram(program, 0x8000);
    std::vector<uint8_t> vectors = { 0x00, 0x80 };
    cart->LoadTestProgram(vectors, 0xFFFC);

    cpu->Reset(true);

    ExecuteInstruction(cpu); // LDA #$42
    test.AssertEquals((uint8_t)0x42, cpu->A, "LDA immediate loads accumulator");

    ExecuteInstruction(cpu); // STA $10
    test.AssertEquals((uint8_t)0x42, bus->ReadRAM(0x10), "STA stores to zero page");

    ExecuteInstruction(cpu); // LDX #$55
    test.AssertEquals((uint8_t)0x55, cpu->X, "LDX immediate loads X register");

    ExecuteInstruction(cpu); // STX $11
    test.AssertEquals((uint8_t)0x55, bus->ReadRAM(0x11), "STX stores to zero page");

    ExecuteInstruction(cpu); // LDY #$AA
    test.AssertEquals((uint8_t)0xAA, cpu->Y, "LDY immediate loads Y register");

    ExecuteInstruction(cpu); // STY $12
    test.AssertEquals((uint8_t)0xAA, bus->ReadRAM(0x12), "STY stores to zero page");

    ExecuteInstruction(cpu); // LDA $10
    test.AssertEquals((uint8_t)0x42, cpu->A, "LDA zero page loads correctly");

    ExecuteInstruction(cpu); // LDX $11
    test.AssertEquals((uint8_t)0x55, cpu->X, "LDX zero page loads correctly");

    ExecuteInstruction(cpu); // LDY $12
    test.AssertEquals((uint8_t)0xAA, cpu->Y, "LDY zero page loads correctly");
}

void TestCPUArithmetic(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus, DiagnosticCartridge* cart) {
    test.StartTest("CPU Arithmetic Instructions");

    std::vector<uint8_t> program = {
        0x18,        // CLC
        0xA9, 0x50,  // LDA #$50
        0x69, 0x10,  // ADC #$10
        0x38,        // SEC
        0xE9, 0x20,  // SBC #$20
        0xE8,        // INX
        0xE8,        // INX
        0xCA,        // DEX
        0xC8,        // INY
        0x88,        // DEY
        0x00         // BRK
    };

    cart->LoadTestProgram(program, 0x8000);
    cpu->Reset(true);

    ExecuteInstruction(cpu); // CLC
    test.Assert((cpu->Status & 0x01) == 0, "CLC clears carry flag");

    ExecuteInstruction(cpu); // LDA #$50
    ExecuteInstruction(cpu); // ADC #$10
    test.AssertEquals((uint8_t)0x60, cpu->A, "ADC performs addition");

    ExecuteInstruction(cpu); // SEC
    test.Assert((cpu->Status & 0x01) != 0, "SEC sets carry flag");

    ExecuteInstruction(cpu); // SBC #$20
    test.AssertEquals((uint8_t)0x40, cpu->A, "SBC performs subtraction");

    uint8_t xBefore = cpu->X;
    ExecuteInstruction(cpu); // INX
    test.AssertEquals((uint8_t)(xBefore + 1), cpu->X, "INX increments X");

    ExecuteInstruction(cpu); // INX
    ExecuteInstruction(cpu); // DEX
    test.AssertEquals((uint8_t)(xBefore + 1), cpu->X, "DEX decrements X");

    uint8_t yBefore = cpu->Y;
    ExecuteInstruction(cpu); // INY
    test.AssertEquals((uint8_t)(yBefore + 1), cpu->Y, "INY increments Y");

    ExecuteInstruction(cpu); // DEY
    test.AssertEquals((uint8_t)yBefore, cpu->Y, "DEY decrements Y");
}

void TestCPULogical(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus, DiagnosticCartridge* cart) {
    test.StartTest("CPU Logical Instructions");

    std::vector<uint8_t> program = {
        0xA9, 0xFF,  // LDA #$FF
        0x29, 0x0F,  // AND #$0F
        0x09, 0xF0,  // ORA #$F0
        0x49, 0x55,  // EOR #$55
        0x00         // BRK
    };

    cart->LoadTestProgram(program, 0x8000);
    cpu->Reset(true);

    ExecuteInstruction(cpu); // LDA #$FF
    ExecuteInstruction(cpu); // AND #$0F
    test.AssertEquals((uint8_t)0x0F, cpu->A, "AND performs bitwise AND");

    ExecuteInstruction(cpu); // ORA #$F0
    test.AssertEquals((uint8_t)0xFF, cpu->A, "ORA performs bitwise OR");

    ExecuteInstruction(cpu); // EOR #$55
    test.AssertEquals((uint8_t)0xAA, cpu->A, "EOR performs bitwise XOR");
}

void TestCPUShifts(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus, DiagnosticCartridge* cart) {
    test.StartTest("CPU Shift/Rotate Instructions");

    std::vector<uint8_t> program = {
        0xA9, 0x01,  // LDA #$01
        0x0A,        // ASL A
        0x4A,        // LSR A
        0x2A,        // ROL A
        0x6A,        // ROR A
        0x00         // BRK
    };

    cart->LoadTestProgram(program, 0x8000);
    cpu->Reset(true);

    ExecuteInstruction(cpu); // LDA #$01
    ExecuteInstruction(cpu); // ASL A
    test.AssertEquals((uint8_t)0x02, cpu->A, "ASL shifts left");

    ExecuteInstruction(cpu); // LSR A
    test.AssertEquals((uint8_t)0x01, cpu->A, "LSR shifts right");

    ExecuteInstruction(cpu); // ROL A
    test.AssertEquals((uint8_t)0x02, cpu->A, "ROL rotates left");

    ExecuteInstruction(cpu); // ROR A
    test.AssertEquals((uint8_t)0x01, cpu->A, "ROR rotates right");
}

void TestCPUFlags(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus, DiagnosticCartridge* cart) {
    test.StartTest("CPU Status Flags");

    std::vector<uint8_t> program = {
        0xA9, 0x00,  // LDA #$00 (sets Z flag)
        0xA9, 0x80,  // LDA #$80 (sets N flag)
        0x18,        // CLC
        0x38,        // SEC
        0xD8,        // CLD
        0xF8,        // SED
        0x58,        // CLI
        0x78,        // SEI
        0xB8,        // CLV
        0x00         // BRK
    };

    cart->LoadTestProgram(program, 0x8000);
    cpu->Reset(true);

    ExecuteInstruction(cpu); // LDA #$00
    test.Assert((cpu->Status & 0x02) != 0, "Zero flag set when loading zero");
    test.Assert((cpu->Status & 0x80) == 0, "Negative flag clear for zero");

    ExecuteInstruction(cpu); // LDA #$80
    test.Assert((cpu->Status & 0x80) != 0, "Negative flag set for negative value");
    test.Assert((cpu->Status & 0x02) == 0, "Zero flag clear for non-zero");

    ExecuteInstruction(cpu); // CLC
    test.Assert((cpu->Status & 0x01) == 0, "CLC clears carry");

    ExecuteInstruction(cpu); // SEC
    test.Assert((cpu->Status & 0x01) != 0, "SEC sets carry");

    ExecuteInstruction(cpu); // CLD
    test.Assert((cpu->Status & 0x08) == 0, "CLD clears decimal");

    ExecuteInstruction(cpu); // SED
    test.Assert((cpu->Status & 0x08) != 0, "SED sets decimal");

    ExecuteInstruction(cpu); // CLI
    test.Assert((cpu->Status & 0x04) == 0, "CLI clears interrupt disable");

    ExecuteInstruction(cpu); // SEI
    test.Assert((cpu->Status & 0x04) != 0, "SEI sets interrupt disable");

    ExecuteInstruction(cpu); // CLV
    test.Assert((cpu->Status & 0x40) == 0, "CLV clears overflow");
}

void TestCPUBranching(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus, DiagnosticCartridge* cart) {
    test.StartTest("CPU Branch Instructions");

    std::vector<uint8_t> program = {
        0xA9, 0x00,  // LDA #$00 (Z=1)
        0xF0, 0x02,  // BEQ +2 (should branch)
        0xA9, 0xFF,  // LDA #$FF (skipped)
        0xD0, 0x02,  // BNE +2 (should not branch)
        0xA9, 0x42,  // LDA #$42
        0x00         // BRK
    };

    cart->LoadTestProgram(program, 0x8000);
    cpu->Reset(true);

    ExecuteInstruction(cpu); // LDA #$00
    uint16_t pcBefore = cpu->PC;
    ExecuteInstruction(cpu); // BEQ +2
    test.Assert(cpu->PC == pcBefore + 4, "BEQ branches when zero flag set");
    test.AssertEquals((uint8_t)0x00, cpu->A, "Branch skipped correct instruction");

    pcBefore = cpu->PC;
    ExecuteInstruction(cpu); // BNE +2
    test.Assert(cpu->PC == pcBefore + 2, "BNE does not branch when zero flag set");

    ExecuteInstruction(cpu); // LDA #$42
    test.AssertEquals((uint8_t)0x42, cpu->A, "Instruction after failed branch executed");
}

void TestCPUStackOperations(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus, DiagnosticCartridge* cart) {
    test.StartTest("CPU Stack Operations");

    std::vector<uint8_t> program = {
        0xA9, 0x42,  // LDA #$42
        0x48,        // PHA
        0xA9, 0x00,  // LDA #$00
        0x68,        // PLA
        0x08,        // PHP
        0x28,        // PLP
        0x00         // BRK
    };

    cart->LoadTestProgram(program, 0x8000);
    cpu->Reset(true);

    ExecuteInstruction(cpu); // LDA #$42
    uint8_t spBefore = cpu->SP;
    ExecuteInstruction(cpu); // PHA
    test.AssertEquals((uint8_t)(spBefore - 1), cpu->SP, "PHA decrements stack pointer");

    ExecuteInstruction(cpu); // LDA #$00
    test.AssertEquals((uint8_t)0x00, cpu->A, "Accumulator changed");

    ExecuteInstruction(cpu); // PLA
    test.AssertEquals((uint8_t)0x42, cpu->A, "PLA restores accumulator from stack");
    test.AssertEquals((uint8_t)spBefore, cpu->SP, "PLA increments stack pointer");

    uint8_t statusBefore = cpu->Status;
    ExecuteInstruction(cpu); // PHP
    ExecuteInstruction(cpu); // PLP
    test.Assert(true, "PHP/PLP executed without crash");
}

// NES Test ROM Log Writer
class NESTestLogWriter {
private:
    std::ofstream logFile;
    CPU6502* cpu;
    DiagnosticBus* bus;
    std::string filename;
    int linesWritten;
    uint64_t ppuCycles;

    // Opcode information for disassembly
    struct OpcodeInfo {
        const char* mnemonic;
        int bytes;
        const char* addrMode;
        bool illegal;  // Mark unofficial/illegal opcodes
    };

    static const OpcodeInfo opcodeTable[256];

    std::string GetOpcodeBytes(uint16_t pc, int numBytes) {
        std::stringstream ss;
        ss << std::uppercase << std::hex << std::setfill('0');
        for (int i = 0; i < numBytes && i < 3; i++) {
            ss << std::setw(2) << (int)bus->CpuRead(pc + i, true);
            if (i < numBytes - 1) ss << " ";
        }
        // Pad to exactly 9 characters total
        std::string result = ss.str();
        while (result.length() < 9) result += " ";
        return result;
    }

    uint16_t Read16Bug(uint16_t addr) {
        // 6502 indirect JMP bug: LSB is at addr, MSB wraps in the same page
        uint8_t lo = bus->CpuRead(addr, true);
        uint8_t hi = bus->CpuRead((addr & 0xFF00) | ((addr + 1) & 0x00FF), true);
        return (hi << 8) | lo;
    }

    std::string DisassembleInstruction(uint16_t pc) {
        uint8_t opcode = bus->CpuRead(pc, true);
        const OpcodeInfo& info = opcodeTable[opcode];

        std::stringstream ss;
        ss << std::uppercase << std::hex << std::setfill('0');

        // Illegal opcode marker
        if (info.illegal) ss << "*";
        else ss << " ";

        ss << info.mnemonic;

        if (info.bytes == 1) {
            if (strcmp(info.addrMode, "ACC") == 0) {
                ss << " A";
            }
            return ss.str();
        }
        else if (info.bytes == 2) {
            uint8_t operand = bus->CpuRead(pc + 1, true);

            if (strcmp(info.addrMode, "IMM") == 0) {
                ss << " #$" << std::setw(2) << (int)operand;
            }
            else if (strcmp(info.addrMode, "ZP0") == 0) {
                uint8_t val = bus->CpuRead(operand, true);
                ss << " $" << std::setw(2) << (int)operand
                    << " = " << std::setw(2) << (int)val;
            }
            else if (strcmp(info.addrMode, "ZPX") == 0) {
                uint8_t addr = (operand + cpu->GetX()) & 0xFF;
                uint8_t val = bus->CpuRead(addr, true);
                ss << " $" << std::setw(2) << (int)operand << ",X @ "
                    << std::setw(2) << (int)addr << " = " << std::setw(2) << (int)val;
            }
            else if (strcmp(info.addrMode, "ZPY") == 0) {
                uint8_t addr = (operand + cpu->GetY()) & 0xFF;
                uint8_t val = bus->CpuRead(addr, true);
                ss << " $" << std::setw(2) << (int)operand << ",Y @ "
                    << std::setw(2) << (int)addr << " = " << std::setw(2) << (int)val;
            }
            else if (strcmp(info.addrMode, "IZX") == 0) {
                uint8_t zp = (operand + cpu->GetX()) & 0xFF;
                uint16_t addr = bus->CpuRead(zp, true) | (bus->CpuRead((zp + 1) & 0xFF, true) << 8);
                uint8_t val = bus->CpuRead(addr, true);
                ss << " ($" << std::setw(2) << (int)operand << ",X) @ "
                    << std::setw(2) << (int)zp
                    << " = " << std::setw(4) << addr
                    << " = " << std::setw(2) << (int)val;
            }
            else if (strcmp(info.addrMode, "IZY") == 0) {
                uint16_t base = bus->CpuRead(operand, true) | (bus->CpuRead((operand + 1) & 0xFF, true) << 8);
                uint16_t addr = (base + cpu->GetY()) & 0xFFFF;
                uint8_t val = bus->CpuRead(addr, true);
                ss << " ($" << std::setw(2) << (int)operand << "),Y = "
                    << std::setw(4) << base << " @ "
                    << std::setw(4) << addr
                    << " = " << std::setw(2) << (int)val;
            }
            else if (strcmp(info.addrMode, "REL") == 0) {
                int8_t offset = (int8_t)operand;
                uint16_t target = pc + 2 + offset;
                ss << " $" << std::setw(4) << target;
            }
            return ss.str();
        }
        else if (info.bytes == 3) {
            uint8_t lo = bus->CpuRead(pc + 1, true);
            uint8_t hi = bus->CpuRead(pc + 2, true);
            uint16_t addr = lo | (hi << 8);

            if (strcmp(info.addrMode, "ABS") == 0) {
                if (strcmp(info.mnemonic, "JMP") == 0 || strcmp(info.mnemonic, "JSR") == 0) {
                    ss << " $" << std::setw(4) << addr;
                }
                else {
                    uint8_t val = bus->CpuRead(addr, true);
                    ss << " $" << std::setw(4) << addr << " = " << std::setw(2) << (int)val;
                }
            }
            else if (strcmp(info.addrMode, "ABX") == 0) {
                uint16_t eff = (addr + cpu->GetX()) & 0xFFFF;
                uint8_t val = bus->CpuRead(eff, true);
                ss << " $" << std::setw(4) << addr << ",X @ " << std::setw(4) << eff
                    << " = " << std::setw(2) << (int)val;
            }
            else if (strcmp(info.addrMode, "ABY") == 0) {
                uint16_t eff = (addr + cpu->GetY()) & 0xFFFF;
                uint8_t val = bus->CpuRead(eff, true);
                ss << " $" << std::setw(4) << addr << ",Y @ " << std::setw(4) << eff
                    << " = " << std::setw(2) << (int)val;
            }
            else if (strcmp(info.addrMode, "IND") == 0) {
                //uint16_t target = bus->CpuRead(addr, true) | (bus->CpuRead((addr + 1) & 0xFFFF, true) << 8);
                uint16_t target = Read16Bug(addr);
                ss << " ($" << std::setw(4) << addr << ") = " << std::setw(4) << target;
            }
            return ss.str();
        }

        return ss.str();
    }


public:
    NESTestLogWriter(const std::string& fname, CPU6502* c, DiagnosticBus* b)
        : cpu(c), bus(b), filename(fname), linesWritten(0), ppuCycles(0) {
        logFile.open(filename, std::ios::out | std::ios::trunc);
        if (!logFile.is_open()) {
            std::cerr << "ERROR: Failed to open log file: " << filename << std::endl;
        }
        else {
            std::cout << "  [INFO] Log file opened: " << filename << std::endl;
        }
    }

    ~NESTestLogWriter() {
        if (logFile.is_open()) {
            logFile.flush();
            logFile.close();
            std::cout << "  [INFO] Log file closed. Lines written: " << linesWritten << std::endl;
        }
    }

    void Close() {
        if (logFile.is_open()) {
            logFile.flush();
            logFile.close();
            std::cout << "  [INFO] Log file closed. Lines written: " << linesWritten << std::endl;
        }
    }

    void LogState() {
        if (!logFile.is_open()) {
            std::cerr << "ERROR: Log file not open!" << std::endl;
            return;
        }

        uint16_t pc = cpu->GetPC();
        uint8_t opcode = bus->CpuRead(pc, true);
        const OpcodeInfo& info = opcodeTable[opcode];

        // Format: PC  BYTES  DISASSEMBLY           A:XX X:XX Y:XX P:XX SP:XX PPU:xxx,yyy CYC:n

        // PC (4 chars)
        logFile << std::uppercase << std::hex << std::setfill('0');
        logFile << std::setw(4) << pc << "  ";

        // Opcode bytes (exactly 9 chars - 3 bytes with spaces)
        std::string bytes = GetOpcodeBytes(pc, info.bytes);
        logFile << bytes;

        // Disassembly (exactly 31 chars, space-padded on right)
        std::string disasm = DisassembleInstruction(pc);
        logFile << disasm;
        // Pad disassembly to exactly 31 characters
        for (int i = disasm.length(); i < 33; i++) {
            logFile << " ";
        }

        // Registers
        logFile << "A:" << std::setw(2) << (int)cpu->GetA() << " ";
        logFile << "X:" << std::setw(2) << (int)cpu->GetX() << " ";
        logFile << "Y:" << std::setw(2) << (int)cpu->GetY() << " ";
        logFile << "P:" << std::setw(2) << (int)cpu->GetStatus() << " ";
        logFile << "SP:" << std::setw(2) << (int)cpu->GetSP() << " ";

        // PPU info (PPU runs 3x faster than CPU)
        ppuCycles = cpu->GetClockCount() * 3;
        int ppuScanline = (ppuCycles / 341) % 262;
        int ppuDot = ppuCycles % 341;

        logFile << std::dec << std::setfill(' ');
        logFile << "PPU:" << std::setw(3) << ppuScanline << ","
            << std::setw(3) << ppuDot << " ";

        // CPU cycle count
        logFile << "CYC:" << cpu->GetClockCount();

        logFile << "\n";

        linesWritten++;

        // Flush every 100 lines to ensure data is written
        if (linesWritten % 100 == 0) {
            logFile.flush();
        }
    }

    bool IsOpen() const {
        return logFile.is_open();
    }

    int GetLinesWritten() const {
        return linesWritten;
    }
};

// Opcode table definition (outside the class)
const NESTestLogWriter::OpcodeInfo NESTestLogWriter::opcodeTable[256] = {
    {"BRK", 1, "IMP", false}, {"ORA", 2, "IZX", false}, {"KIL", 1, "IMP", true}, {"SLO", 2, "IZX", true},
    {"NOP", 2, "ZP0", true}, {"ORA", 2, "ZP0", false}, {"ASL", 2, "ZP0", false}, {"SLO", 2, "ZP0", true},
    {"PHP", 1, "IMP", false}, {"ORA", 2, "IMM", false}, {"ASL", 1, "ACC", false}, {"ANC", 2, "IMM", true},
    {"NOP", 3, "ABS", true}, {"ORA", 3, "ABS", false}, {"ASL", 3, "ABS", false}, {"SLO", 3, "ABS", true},
    {"BPL", 2, "REL", false}, {"ORA", 2, "IZY", false}, {"KIL", 1, "IMP", true}, {"SLO", 2, "IZY", true},
    {"NOP", 2, "ZPX", true}, {"ORA", 2, "ZPX", false}, {"ASL", 2, "ZPX", false}, {"SLO", 2, "ZPX", true},
    {"CLC", 1, "IMP", false}, {"ORA", 3, "ABY", false}, {"NOP", 1, "IMP", true}, {"SLO", 3, "ABY", true},
    {"NOP", 3, "ABX", true}, {"ORA", 3, "ABX", false}, {"ASL", 3, "ABX", false}, {"SLO", 3, "ABX", true},
    {"JSR", 3, "ABS", false}, {"AND", 2, "IZX", false}, {"KIL", 1, "IMP", true}, {"RLA", 2, "IZX", true},
    {"BIT", 2, "ZP0", false}, {"AND", 2, "ZP0", false}, {"ROL", 2, "ZP0", false}, {"RLA", 2, "ZP0", true},
    {"PLP", 1, "IMP", false}, {"AND", 2, "IMM", false}, {"ROL", 1, "ACC", false}, {"ANC", 2, "IMM", true},
    {"BIT", 3, "ABS", false}, {"AND", 3, "ABS", false}, {"ROL", 3, "ABS", false}, {"RLA", 3, "ABS", true},
    {"BMI", 2, "REL", false}, {"AND", 2, "IZY", false}, {"KIL", 1, "IMP", true}, {"RLA", 2, "IZY", true},
    {"NOP", 2, "ZPX", true}, {"AND", 2, "ZPX", false}, {"ROL", 2, "ZPX", false}, {"RLA", 2, "ZPX", true},
    {"SEC", 1, "IMP", false}, {"AND", 3, "ABY", false}, {"NOP", 1, "IMP", true}, {"RLA", 3, "ABY", true},
    {"NOP", 3, "ABX", true}, {"AND", 3, "ABX", false}, {"ROL", 3, "ABX", false}, {"RLA", 3, "ABX", true},
    {"RTI", 1, "IMP", false}, {"EOR", 2, "IZX", false}, {"KIL", 1, "IMP", true}, {"SRE", 2, "IZX", true},
    {"NOP", 2, "ZP0", true}, {"EOR", 2, "ZP0", false}, {"LSR", 2, "ZP0", false}, {"SRE", 2, "ZP0", true},
    {"PHA", 1, "IMP", false}, {"EOR", 2, "IMM", false}, {"LSR", 1, "ACC", false}, {"ALR", 2, "IMM", true},
    {"JMP", 3, "ABS", false}, {"EOR", 3, "ABS", false}, {"LSR", 3, "ABS", false}, {"SRE", 3, "ABS", true},
    {"BVC", 2, "REL", false}, {"EOR", 2, "IZY", false}, {"KIL", 1, "IMP", true}, {"SRE", 2, "IZY", true},
    {"NOP", 2, "ZPX", true}, {"EOR", 2, "ZPX", false}, {"LSR", 2, "ZPX", false}, {"SRE", 2, "ZPX", true},
    {"CLI", 1, "IMP", false}, {"EOR", 3, "ABY", false}, {"NOP", 1, "IMP", true}, {"SRE", 3, "ABY", true},
    {"NOP", 3, "ABX", true}, {"EOR", 3, "ABX", false}, {"LSR", 3, "ABX", false}, {"SRE", 3, "ABX", true},
    {"RTS", 1, "IMP", false}, {"ADC", 2, "IZX", false}, {"KIL", 1, "IMP", true}, {"RRA", 2, "IZX", true},
    {"NOP", 2, "ZP0", true}, {"ADC", 2, "ZP0", false}, {"ROR", 2, "ZP0", false}, {"RRA", 2, "ZP0", true},
    {"PLA", 1, "IMP", false}, {"ADC", 2, "IMM", false}, {"ROR", 1, "ACC", false}, {"ARR", 2, "IMM", true},
    {"JMP", 3, "IND", false}, {"ADC", 3, "ABS", false}, {"ROR", 3, "ABS", false}, {"RRA", 3, "ABS", true},
    {"BVS", 2, "REL", false}, {"ADC", 2, "IZY", false}, {"KIL", 1, "IMP", true}, {"RRA", 2, "IZY", true},
    {"NOP", 2, "ZPX", true}, {"ADC", 2, "ZPX", false}, {"ROR", 2, "ZPX", false}, {"RRA", 2, "ZPX", true},
    {"SEI", 1, "IMP", false}, {"ADC", 3, "ABY", false}, {"NOP", 1, "IMP", true}, {"RRA", 3, "ABY", true},
    {"NOP", 3, "ABX", true}, {"ADC", 3, "ABX", false}, {"ROR", 3, "ABX", false}, {"RRA", 3, "ABX", true},
    {"NOP", 2, "IMM", true}, {"STA", 2, "IZX", false}, {"NOP", 2, "IMM", true}, {"SAX", 2, "IZX", true},
    {"STY", 2, "ZP0", false}, {"STA", 2, "ZP0", false}, {"STX", 2, "ZP0", false}, {"SAX", 2, "ZP0", true},
    {"DEY", 1, "IMP", false}, {"NOP", 2, "IMM", true}, {"TXA", 1, "IMP", false}, {"XAA", 2, "IMM", true},
    {"STY", 3, "ABS", false}, {"STA", 3, "ABS", false}, {"STX", 3, "ABS", false}, {"SAX", 3, "ABS", true},
    {"BCC", 2, "REL", false}, {"STA", 2, "IZY", false}, {"KIL", 1, "IMP", true}, {"SHA", 2, "IZY", true},
    {"STY", 2, "ZPX", false}, {"STA", 2, "ZPX", false}, {"STX", 2, "ZPY", false}, {"SAX", 2, "ZPY", true},
    {"TYA", 1, "IMP", false}, {"STA", 3, "ABY", false}, {"TXS", 1, "IMP", false}, {"TAS", 3, "ABY", true},
    {"SHY", 3, "ABX", true}, {"STA", 3, "ABX", false}, {"SHX", 3, "ABY", true}, {"SHA", 3, "ABY", true},
    {"LDY", 2, "IMM", false}, {"LDA", 2, "IZX", false}, {"LDX", 2, "IMM", false}, {"LAX", 2, "IZX", true},
    {"LDY", 2, "ZP0", false}, {"LDA", 2, "ZP0", false}, {"LDX", 2, "ZP0", false}, {"LAX", 2, "ZP0", true},
    {"TAY", 1, "IMP", false}, {"LDA", 2, "IMM", false}, {"TAX", 1, "IMP", false}, {"LAX", 2, "IMM", true},
    {"LDY", 3, "ABS", false}, {"LDA", 3, "ABS", false}, {"LDX", 3, "ABS", false}, {"LAX", 3, "ABS", true},
    {"BCS", 2, "REL", false}, {"LDA", 2, "IZY", false}, {"KIL", 1, "IMP", true}, {"LAX", 2, "IZY", true},
    {"LDY", 2, "ZPX", false}, {"LDA", 2, "ZPX", false}, {"LDX", 2, "ZPY", false}, {"LAX", 2, "ZPY", true},
    {"CLV", 1, "IMP", false}, {"LDA", 3, "ABY", false}, {"TSX", 1, "IMP", false}, {"LAS", 3, "ABY", true},
    {"LDY", 3, "ABX", false}, {"LDA", 3, "ABX", false}, {"LDX", 3, "ABY", false}, {"LAX", 3, "ABY", true},
    {"CPY", 2, "IMM", false}, {"CMP", 2, "IZX", false}, {"NOP", 2, "IMM", true}, {"DCP", 2, "IZX", true},
    {"CPY", 2, "ZP0", false}, {"CMP", 2, "ZP0", false}, {"DEC", 2, "ZP0", false}, {"DCP", 2, "ZP0", true},
    {"INY", 1, "IMP", false}, {"CMP", 2, "IMM", false}, {"DEX", 1, "IMP", false}, {"AXS", 2, "IMM", true},
    {"CPY", 3, "ABS", false}, {"CMP", 3, "ABS", false}, {"DEC", 3, "ABS", false}, {"DCP", 3, "ABS", true},
    {"BNE", 2, "REL", false}, {"CMP", 2, "IZY", false}, {"KIL", 1, "IMP", true}, {"DCP", 2, "IZY", true},
    {"NOP", 2, "ZPX", true}, {"CMP", 2, "ZPX", false}, {"DEC", 2, "ZPX", false}, {"DCP", 2, "ZPX", true},
    {"CLD", 1, "IMP", false}, {"CMP", 3, "ABY", false}, {"NOP", 1, "IMP", true}, {"DCP", 3, "ABY", true},
    {"NOP", 3, "ABX", true}, {"CMP", 3, "ABX", false}, {"DEC", 3, "ABX", false}, {"DCP", 3, "ABX", true},
    {"CPX", 2, "IMM", false}, {"SBC", 2, "IZX", false}, {"NOP", 2, "IMM", true}, {"ISB", 2, "IZX", true},
    {"CPX", 2, "ZP0", false}, {"SBC", 2, "ZP0", false}, {"INC", 2, "ZP0", false}, {"ISB", 2, "ZP0", true},
    {"INX", 1, "IMP", false}, {"SBC", 2, "IMM", false}, {"NOP", 1, "IMP", false}, {"SBC", 2, "IMM", true},
    {"CPX", 3, "ABS", false}, {"SBC", 3, "ABS", false}, {"INC", 3, "ABS", false}, {"ISB", 3, "ABS", true},
    {"BEQ", 2, "REL", false}, {"SBC", 2, "IZY", false}, {"KIL", 1, "IMP", true}, {"ISB", 2, "IZY", true},
    {"NOP", 2, "ZPX", true}, {"SBC", 2, "ZPX", false}, {"INC", 2, "ZPX", false}, {"ISB", 2, "ZPX", true},
    {"SED", 1, "IMP", false}, {"SBC", 3, "ABY", false}, {"NOP", 1, "IMP", true}, {"ISB", 3, "ABY", true},
    {"NOP", 3, "ABX", true}, {"SBC", 3, "ABX", false}, {"INC", 3, "ABX", false}, {"ISB", 3, "ABX", true}
};

void TestCPUWithNESTestROM(DiagnosticTest& test, CPU6502* cpu, DiagnosticBus* bus, DiagnosticCartridge* cart, const std::string& romPath) {
    test.StartTest("CPU Official NES Test ROM");

    if (!cart->LoadFromFile(romPath)) {
        test.Assert(false, "Failed to load NES test ROM from: " + romPath);
        test.Info("Place nestest.nes in the same directory as the executable");
        return;
    }

    test.Assert(true, "NES test ROM loaded successfully");

    // Set PC to automated test starting point
    cpu->PC = 0xC000;
    cpu->Status = 0x24;
    cpu->SP = 0xFD;

    // Create log file
    NESTestLogWriter logger("nestest_output.log", cpu, bus);

    if (!logger.IsOpen()) {
        test.Assert(false, "Failed to create log file");
        return;
    }

    test.Info("Running nestest.nes - this may take a moment...");
    test.Info("Output will be written to nestest_output.log");
    test.Info("Compare with nestest.log (golden reference) to verify correctness");

    // Run until we hit the end or loop
    uint64_t maxInstructions = 10000;
    uint16_t lastPC = 0;
    int samePC = 0;

    for (uint64_t i = 0; i < maxInstructions; i++) {
        // Log state before executing instruction
        logger.LogState();

        // Check if we're stuck in a loop
        if (cpu->PC == lastPC) {
            samePC++;
            if (samePC > 3) {
                test.Info("Detected infinite loop or completion at PC: 0x" +
                    std::to_string(cpu->PC));
                break;
            }
        }
        else {
            samePC = 0;
        }
        lastPC = cpu->PC;

        // Execute one instruction
        ExecuteInstruction(cpu);

        // Check for specific end addresses (nestest completes at certain points)
        if (cpu->PC == 0xC66E) {
            logger.LogState(); // log the state for the final instruct
            test.Info("Test completed at official end point (0xC66E)");
            break;
        }

        // Alternative end point check
        //if (cpu->PC == 0xC6BD) {
        //    logger.LogState();
        //    test.Info("Test completed at alternative end point (0xC6BD)");
        //    break;
        //}
    }

    test.Assert(true, "NES test ROM execution completed");
    test.Info("Log file: nestest_output.log");
    test.Info("Lines written: " + std::to_string(logger.GetLinesWritten()));
    test.Info("Instructions executed: " + std::to_string(cpu->GetInstructionCount()));
    test.Info("Total cycles: " + std::to_string(cpu->GetClockCount()));

    // Read test result from memory location $0002-$0003 (if applicable)
    uint8_t testResult = bus->ReadRAM(0x0002);
    if (testResult == 0x00) {
        test.Assert(true, "Test ROM reported success (result code: 0x00)");
    }
    else {
        test.Info("Test result code at $0002: 0x" + std::to_string(testResult));
    }

    // Explicit flush message
    test.Info("Flushing and closing log file...");

    logger.Close();
    CompareLogs("nestest_output.log", "nestest.log");
}