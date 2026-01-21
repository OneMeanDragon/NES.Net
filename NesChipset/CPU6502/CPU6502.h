#pragma once

#include <cstdint>
#include <functional>

#ifdef _WIN32
    #define DLLEXPORT extern "C" __declspec(dllexport)
#else
    #define DLLEXPORT
#endif

// Forward declaration
class NESBus;

class CPU6502 {
private:
    enum
    {
        NMI_VECTOR = 0xFFFA,
        RESET_VECTOR = 0xFFFC,
        IRQ_VECTOR = 0xFFFE,
        RESET_CYCLES = 7,
        INT_CYCLES = 7,
        BRK_CYCLES = 7,
        RTI_CYCLES = 6,
        RTS_CYCLES = 6,
        PHA_CYCLES = 3,
        PHP_CYCLES = 3,
        PLA_CYCLES = 4,
        PLP_CYCLES = 4,
        JSR_CYCLES = 6,
        JMP_ABS_CYCLES = 3,
        JMP_IND_CYCLES = 5
    };
public:
    CPU6502();
    ~CPU6502();

    // Lifecycle
    void Reset(bool coldstart);
    void Clock();

    // Interrupts
    void IRQ();
    void NMI();

    // Bus connection
    void ConnectBus(NESBus* bus);

    // Status
    bool IsComplete() const { return _cycles == 0; }

    // Debug/inspection
    uint16_t GetPC() const { return PC; }
    uint8_t GetA() const { return A; }
    uint8_t GetX() const { return X; }
    uint8_t GetY() const { return Y; }
    uint8_t GetSP() const { return SP; }
    uint8_t GetStatus() const { return Status; }
    uint64_t GetClockCount() const { return ClockCount; }
    uint64_t GetInstructionCount() const { return InstructionCount; }

    // CPU Registers (public for direct access if needed)
    uint8_t A;              // Accumulator
    uint8_t X;              // X Register
    uint8_t Y;              // Y Register
    uint8_t SP;             // Stack Pointer
    uint16_t PC;            // Program Counter
    uint8_t Status;         // Status Register

    uint64_t ClockCount;
    uint64_t InstructionCount;

private:
    // Status flags
    enum StatusFlags : uint8_t {
        C = 1 << 0,  // Carry
        Z = 1 << 1,  // Zero
        I = 1 << 2,  // Interrupt Disable
        D = 1 << 3,  // Decimal mode (not supported on the N2A03)
        B = 1 << 4,  // Break
        U = 1 << 5,  // Unused (always 1)
        V = 1 << 6,  // Overflow
        N = 1 << 7   // Negative
    };

    // Addressing modes
    enum class AddrMode {
        NONE, IMP, IMM, ZP0, ZPX, ZPY,
        ABS, ABX, ABY, IND, IZX, IZY, REL
    };

    // Instruction structure
    struct Instruction {
        const char* name;
        uint8_t(CPU6502::* operate)();
        uint8_t(CPU6502::* addressMode)();
        AddrMode modeType;
        uint8_t cycles;
    };

    // Instruction table
    Instruction _instructions[256];

    // Internal state
    uint8_t _fetched;
    uint16_t _addrAbs;
    uint16_t _addrAbs_Base;
    uint16_t _addrRel;
    uint8_t _opcode;
    uint8_t _cycles;
    uint16_t _temp;

    // Bus connection
    NESBus* _bus;

    // Bus operations
    uint8_t Read(uint16_t addr);
    void Write(uint16_t addr, uint8_t data);

    // Flag operations
    uint8_t GetFlag(StatusFlags flag);
    void SetFlag(StatusFlags flag, bool value);

    // Stack operations
    void Push(uint8_t data);
    uint8_t Pop();
    void PushWord(uint16_t data);
    uint16_t PopWord();

    // Fetch helper
    uint8_t Fetch();

    // Branch helper
    uint8_t Branch(bool condition);

    // Initialize instruction table
    void InitializeInstructionTable();

    // Addressing modes
    uint8_t IMP(); uint8_t IMM(); uint8_t ZP0(); uint8_t ZPX(); uint8_t ZPY();
    uint8_t REL(); uint8_t ABS(); uint8_t ABX(); uint8_t ABY(); uint8_t IND();
    uint8_t IZX(); uint8_t IZY();

    // Official instructions
    uint8_t ADC(); uint8_t AND(); uint8_t ASL(); uint8_t BCC(); uint8_t BCS();
    uint8_t BEQ(); uint8_t BIT(); uint8_t BMI(); uint8_t BNE(); uint8_t BPL();
    uint8_t BRK(); uint8_t BVC(); uint8_t BVS(); uint8_t CLC(); uint8_t CLD();
    uint8_t CLI(); uint8_t CLV(); uint8_t CMP(); uint8_t CPX(); uint8_t CPY();
    uint8_t DEC(); uint8_t DEX(); uint8_t DEY(); uint8_t EOR(); uint8_t INC();
    uint8_t INX(); uint8_t INY(); uint8_t JMP(); uint8_t JSR(); uint8_t LDA();
    uint8_t LDX(); uint8_t LDY(); uint8_t LSR(); uint8_t NOP(); uint8_t ORA();
    uint8_t PHA(); uint8_t PHP(); uint8_t PLA(); uint8_t PLP(); uint8_t ROL();
    uint8_t ROR(); uint8_t RTI(); uint8_t RTS(); uint8_t SBC(); uint8_t SEC();
    uint8_t SED(); uint8_t SEI(); uint8_t STA(); uint8_t STX(); uint8_t STY();
    uint8_t TAX(); uint8_t TAY(); uint8_t TSX(); uint8_t TXA(); uint8_t TXS();
    uint8_t TYA();

    // Illegal instructions
    uint8_t KIL(); uint8_t LAX(); uint8_t SAX(); uint8_t DCP(); uint8_t ISB();
    uint8_t SLO(); uint8_t RLA(); uint8_t SRE(); uint8_t RRA(); uint8_t ANC();
    uint8_t ALR(); uint8_t ARR(); uint8_t AXS(); uint8_t XAA(); uint8_t LAS();
    uint8_t SHA(); uint8_t SHX(); uint8_t SHY(); uint8_t TAS(); uint8_t ATX();
};

// 6502 exports
DLLEXPORT CPU6502* CreateCPU();
DLLEXPORT void DestroyCPU(CPU6502* cpu);

DLLEXPORT void CPU_Reset(CPU6502* cpu, bool coldstart);
DLLEXPORT void CPU_Clock(CPU6502* cpu);
DLLEXPORT void CPU_IRQ(CPU6502* cpu);
DLLEXPORT void CPU_NMI(CPU6502* cpu);

DLLEXPORT void CPU_ConnectBus(CPU6502* cpu, NESBus* bus);

DLLEXPORT bool CPU_IsComplete(CPU6502* cpu);
DLLEXPORT uint16_t CPU_GetPC(CPU6502* cpu);
DLLEXPORT uint8_t CPU_GetA(CPU6502* cpu);
DLLEXPORT uint8_t CPU_GetX(CPU6502* cpu);
DLLEXPORT uint8_t CPU_GetY(CPU6502* cpu);
DLLEXPORT uint8_t CPU_GetSP(CPU6502* cpu);
DLLEXPORT uint8_t CPU_GetStatus(CPU6502* cpu);
DLLEXPORT uint64_t CPU_GetClockCount(CPU6502* cpu);

// Aliases for Bus compatibility
DLLEXPORT void ClockCPU(CPU6502* cpu);
DLLEXPORT void ResetCPU(CPU6502* cpu, bool coldstart);
DLLEXPORT void TriggerNMI(CPU6502* cpu);
DLLEXPORT void TriggerIRQ(CPU6502* cpu);