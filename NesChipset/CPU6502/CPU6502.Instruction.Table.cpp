#include "CPU6502.h"

void CPU6502::InitializeInstructionTable() {
    // Initialize all to KIL by default
    for (int i = 0; i < 256; i++) {
        _instructions[i] = { "KIL", &CPU6502::KIL, &CPU6502::IMP, AddrMode::IMP, 2 };
    }

    //' $00-$0F
    _instructions[0x00] = { "BRK", &CPU6502::BRK, &CPU6502::IMM, AddrMode::IMM, 7 };
    _instructions[0x01] = { "ORA", &CPU6502::ORA, &CPU6502::IZX, AddrMode::IZX, 6 };

    _instructions[0x03] = { "SLO", &CPU6502::SLO, &CPU6502::IZX, AddrMode::IZX, 8 };
    _instructions[0x04] = { "NOP", &CPU6502::NOP, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x05] = { "ORA", &CPU6502::ORA, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x06] = { "ASL", &CPU6502::ASL, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0x07] = { "SLO", &CPU6502::SLO, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0x08] = { "PHP", &CPU6502::PHP, &CPU6502::IMP, AddrMode::IMP, 3 };
    _instructions[0x09] = { "ORA", &CPU6502::ORA, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x0A] = { "ASL", &CPU6502::ASL, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x0B] = { "ANC", &CPU6502::ANC, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x0C] = { "NOP", &CPU6502::NOP, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x0D] = { "ORA", &CPU6502::ORA, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x0E] = { "ASL", &CPU6502::ASL, &CPU6502::ABS, AddrMode::ABS, 6 };
    _instructions[0x0F] = { "SLO", &CPU6502::SLO, &CPU6502::ABS, AddrMode::ABS, 6 };

    //' $10-$1F
    _instructions[0x10] = { "BPL", &CPU6502::BPL, &CPU6502::REL, AddrMode::REL, 2 };
    _instructions[0x11] = { "ORA", &CPU6502::ORA, &CPU6502::IZY, AddrMode::IZY, 5 };

    _instructions[0x13] = { "SLO", &CPU6502::SLO, &CPU6502::IZY, AddrMode::IZY, 8 };
    _instructions[0x14] = { "NOP", &CPU6502::NOP, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x15] = { "ORA", &CPU6502::ORA, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x16] = { "ASL", &CPU6502::ASL, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0x17] = { "SLO", &CPU6502::SLO, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0x18] = { "CLC", &CPU6502::CLC, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x19] = { "ORA", &CPU6502::ORA, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0x1A] = { "NOP", &CPU6502::NOP, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x1B] = { "SLO", &CPU6502::SLO, &CPU6502::ABY, AddrMode::ABY, 7 };
    _instructions[0x1C] = { "NOP", &CPU6502::NOP, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0x1D] = { "ORA", &CPU6502::ORA, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0x1E] = { "ASL", &CPU6502::ASL, &CPU6502::ABX, AddrMode::ABX, 7 };
    _instructions[0x1F] = { "SLO", &CPU6502::SLO, &CPU6502::ABX, AddrMode::ABX, 7 };

    //' $20-$2F
    _instructions[0x20] = { "JSR", &CPU6502::JSR, &CPU6502::ABS, AddrMode::ABS, 6 };
    _instructions[0x21] = { "AND", &CPU6502::AND, &CPU6502::IZX, AddrMode::IZX, 6 };

    _instructions[0x23] = { "RLA", &CPU6502::RLA, &CPU6502::IZX, AddrMode::IZX, 8 };
    _instructions[0x24] = { "BIT", &CPU6502::BIT, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x25] = { "AND", &CPU6502::AND, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x26] = { "ROL", &CPU6502::ROL, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0x27] = { "RLA", &CPU6502::RLA, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0x28] = { "PLP", &CPU6502::PLP, &CPU6502::IMP, AddrMode::IMP, 4 };
    _instructions[0x29] = { "AND", &CPU6502::AND, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x2A] = { "ROL", &CPU6502::ROL, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x2B] = { "ANC", &CPU6502::ANC, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x2C] = { "BIT", &CPU6502::BIT, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x2D] = { "AND", &CPU6502::AND, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x2E] = { "ROL", &CPU6502::ROL, &CPU6502::ABS, AddrMode::ABS, 6 };
    _instructions[0x2F] = { "RLA", &CPU6502::RLA, &CPU6502::ABS, AddrMode::ABS, 6 };

    //' $30-$3F
    _instructions[0x30] = { "BMI", &CPU6502::BMI, &CPU6502::REL, AddrMode::REL, 2 };
    _instructions[0x31] = { "AND", &CPU6502::AND, &CPU6502::IZY, AddrMode::IZY, 5 };

    _instructions[0x33] = { "RLA", &CPU6502::RLA, &CPU6502::IZY, AddrMode::IZY, 8 };
    _instructions[0x34] = { "NOP", &CPU6502::NOP, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x35] = { "AND", &CPU6502::AND, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x36] = { "ROL", &CPU6502::ROL, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0x37] = { "RLA", &CPU6502::RLA, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0x38] = { "SEC", &CPU6502::SEC, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x39] = { "AND", &CPU6502::AND, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0x3A] = { "NOP", &CPU6502::NOP, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x3B] = { "RLA", &CPU6502::RLA, &CPU6502::ABY, AddrMode::ABY, 7 };
    _instructions[0x3C] = { "NOP", &CPU6502::NOP, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0x3D] = { "AND", &CPU6502::AND, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0x3E] = { "ROL", &CPU6502::ROL, &CPU6502::ABX, AddrMode::ABX, 7 };
    _instructions[0x3F] = { "RLA", &CPU6502::RLA, &CPU6502::ABX, AddrMode::ABX, 7 };

    //' $40-$4F
    _instructions[0x40] = { "RTI", &CPU6502::RTI, &CPU6502::IMP, AddrMode::IMP, 6 };
    _instructions[0x41] = { "EOR", &CPU6502::EOR, &CPU6502::IZX, AddrMode::IZX, 6 };

    _instructions[0x43] = { "SRE", &CPU6502::SRE, &CPU6502::IZX, AddrMode::IZX, 8 };
    _instructions[0x44] = { "NOP", &CPU6502::NOP, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x45] = { "EOR", &CPU6502::EOR, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x46] = { "LSR", &CPU6502::LSR, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0x47] = { "SRE", &CPU6502::SRE, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0x48] = { "PHA", &CPU6502::PHA, &CPU6502::IMP, AddrMode::IMP, 3 };
    _instructions[0x49] = { "EOR", &CPU6502::EOR, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x4A] = { "LSR", &CPU6502::LSR, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x4B] = { "ALR", &CPU6502::ALR, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x4C] = { "JMP", &CPU6502::JMP, &CPU6502::ABS, AddrMode::ABS, 3 };
    _instructions[0x4D] = { "EOR", &CPU6502::EOR, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x4E] = { "LSR", &CPU6502::LSR, &CPU6502::ABS, AddrMode::ABS, 6 };
    _instructions[0x4F] = { "SRE", &CPU6502::SRE, &CPU6502::ABS, AddrMode::ABS, 6 };

    //' $50-$5F
    _instructions[0x50] = { "BVC", &CPU6502::BVC, &CPU6502::REL, AddrMode::REL, 2 };
    _instructions[0x51] = { "EOR", &CPU6502::EOR, &CPU6502::IZY, AddrMode::IZY, 5 };

    _instructions[0x53] = { "SRE", &CPU6502::SRE, &CPU6502::IZY, AddrMode::IZY, 8 };
    _instructions[0x54] = { "NOP", &CPU6502::NOP, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x55] = { "EOR", &CPU6502::EOR, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x56] = { "LSR", &CPU6502::LSR, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0x57] = { "SRE", &CPU6502::SRE, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0x58] = { "CLI", &CPU6502::CLI, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x59] = { "EOR", &CPU6502::EOR, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0x5A] = { "NOP", &CPU6502::NOP, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x5B] = { "SRE", &CPU6502::SRE, &CPU6502::ABY, AddrMode::ABY, 7 };
    _instructions[0x5C] = { "NOP", &CPU6502::NOP, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0x5D] = { "EOR", &CPU6502::EOR, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0x5E] = { "LSR", &CPU6502::LSR, &CPU6502::ABX, AddrMode::ABX, 7 };
    _instructions[0x5F] = { "SRE", &CPU6502::SRE, &CPU6502::ABX, AddrMode::ABX, 7 };

    //' $60-$6F
    _instructions[0x60] = { "RTS", &CPU6502::RTS, &CPU6502::IMP, AddrMode::IMP, 6 };
    _instructions[0x61] = { "ADC", &CPU6502::ADC, &CPU6502::IZX, AddrMode::IZX, 6 };

    _instructions[0x63] = { "RRA", &CPU6502::RRA, &CPU6502::IZX, AddrMode::IZX, 8 };
    _instructions[0x64] = { "NOP", &CPU6502::NOP, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x65] = { "ADC", &CPU6502::ADC, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x66] = { "ROR", &CPU6502::ROR, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0x67] = { "RRA", &CPU6502::RRA, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0x68] = { "PLA", &CPU6502::PLA, &CPU6502::IMP, AddrMode::IMP, 4 };
    _instructions[0x69] = { "ADC", &CPU6502::ADC, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x6A] = { "ROR", &CPU6502::ROR, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x6B] = { "ARR", &CPU6502::ARR, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x6C] = { "JMP", &CPU6502::JMP, &CPU6502::IND, AddrMode::IND, 5 };
    _instructions[0x6D] = { "ADC", &CPU6502::ADC, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x6E] = { "ROR", &CPU6502::ROR, &CPU6502::ABS, AddrMode::ABS, 6 };
    _instructions[0x6F] = { "RRA", &CPU6502::RRA, &CPU6502::ABS, AddrMode::ABS, 6 };

    //' $70-$7F
    _instructions[0x70] = { "BVS", &CPU6502::BVS, &CPU6502::REL, AddrMode::REL, 2 };
    _instructions[0x71] = { "ADC", &CPU6502::ADC, &CPU6502::IZY, AddrMode::IZY, 5 };

    _instructions[0x73] = { "RRA", &CPU6502::RRA, &CPU6502::IZY, AddrMode::IZY, 8 };
    _instructions[0x74] = { "NOP", &CPU6502::NOP, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x75] = { "ADC", &CPU6502::ADC, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x76] = { "ROR", &CPU6502::ROR, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0x77] = { "RRA", &CPU6502::RRA, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0x78] = { "SEI", &CPU6502::SEI, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x79] = { "ADC", &CPU6502::ADC, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0x7A] = { "NOP", &CPU6502::NOP, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x7B] = { "RRA", &CPU6502::RRA, &CPU6502::ABY, AddrMode::ABY, 7 };
    _instructions[0x7C] = { "NOP", &CPU6502::NOP, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0x7D] = { "ADC", &CPU6502::ADC, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0x7E] = { "ROR", &CPU6502::ROR, &CPU6502::ABX, AddrMode::ABX, 7 };
    _instructions[0x7F] = { "RRA", &CPU6502::RRA, &CPU6502::ABX, AddrMode::ABX, 7 };

    //' $80-$8F
    _instructions[0x80] = { "NOP", &CPU6502::NOP, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x81] = { "STA", &CPU6502::STA, &CPU6502::IZX, AddrMode::IZX, 6 };
    _instructions[0x82] = { "NOP", &CPU6502::NOP, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x83] = { "SAX", &CPU6502::SAX, &CPU6502::IZX, AddrMode::IZX, 6 };
    _instructions[0x84] = { "STY", &CPU6502::STY, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x85] = { "STA", &CPU6502::STA, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x86] = { "STX", &CPU6502::STX, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x87] = { "SAX", &CPU6502::SAX, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0x88] = { "DEY", &CPU6502::DEY, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x89] = { "NOP", &CPU6502::NOP, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x8A] = { "TXA", &CPU6502::TXA, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x8B] = { "XAA", &CPU6502::XAA, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0x8C] = { "STY", &CPU6502::STY, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x8D] = { "STA", &CPU6502::STA, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x8E] = { "STX", &CPU6502::STX, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0x8F] = { "SAX", &CPU6502::SAX, &CPU6502::ABS, AddrMode::ABS, 4 };

    //' $90-$9F
    _instructions[0x90] = { "BCC", &CPU6502::BCC, &CPU6502::REL, AddrMode::REL, 2 };
    _instructions[0x91] = { "STA", &CPU6502::STA, &CPU6502::IZY, AddrMode::IZY, 6 };

    _instructions[0x93] = { "SHA", &CPU6502::SHA, &CPU6502::IZY, AddrMode::IZY, 6 };
    _instructions[0x94] = { "STY", &CPU6502::STY, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x95] = { "STA", &CPU6502::STA, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0x96] = { "STX", &CPU6502::STX, &CPU6502::ZPY, AddrMode::ZPY, 4 };
    _instructions[0x97] = { "SAX", &CPU6502::SAX, &CPU6502::ZPY, AddrMode::ZPY, 4 };
    _instructions[0x98] = { "TYA", &CPU6502::TYA, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x99] = { "STA", &CPU6502::STA, &CPU6502::ABY, AddrMode::ABY, 5 };
    _instructions[0x9A] = { "TXS", &CPU6502::TXS, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0x9B] = { "TAS", &CPU6502::TAS, &CPU6502::ABY, AddrMode::ABY, 5 };
    _instructions[0x9C] = { "SHY", &CPU6502::SHY, &CPU6502::ABX, AddrMode::ABX, 5 };
    _instructions[0x9D] = { "STA", &CPU6502::STA, &CPU6502::ABX, AddrMode::ABX, 5 };
    _instructions[0x9E] = { "SHX", &CPU6502::SHX, &CPU6502::ABY, AddrMode::ABY, 5 };
    _instructions[0x9F] = { "SHA", &CPU6502::SHA, &CPU6502::ABY, AddrMode::ABY, 5 };

    //' $A0-$AF
    _instructions[0xA0] = { "LDY", &CPU6502::LDY, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xA1] = { "LDA", &CPU6502::LDA, &CPU6502::IZX, AddrMode::IZX, 6 };
    _instructions[0xA2] = { "LDX", &CPU6502::LDX, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xA3] = { "LAX", &CPU6502::LAX, &CPU6502::IZX, AddrMode::IZX, 6 };
    _instructions[0xA4] = { "LDY", &CPU6502::LDY, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0xA5] = { "LDA", &CPU6502::LDA, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0xA6] = { "LDX", &CPU6502::LDX, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0xA7] = { "LAX", &CPU6502::LAX, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0xA8] = { "TAY", &CPU6502::TAY, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xA9] = { "LDA", &CPU6502::LDA, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xAA] = { "TAX", &CPU6502::TAX, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xAB] = { "ARR", &CPU6502::ARR, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xAC] = { "LDY", &CPU6502::LDY, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0xAD] = { "LDA", &CPU6502::LDA, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0xAE] = { "LDX", &CPU6502::LDX, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0xAF] = { "LAX", &CPU6502::LAX, &CPU6502::ABS, AddrMode::ABS, 4 };

    //' $B0-$BF
    _instructions[0xB0] = { "BCS", &CPU6502::BCS, &CPU6502::REL, AddrMode::REL, 2 };
    _instructions[0xB1] = { "LDA", &CPU6502::LDA, &CPU6502::IZY, AddrMode::IZY, 5 };

    _instructions[0xB3] = { "LAX", &CPU6502::LAX, &CPU6502::IZY, AddrMode::IZY, 5 };
    _instructions[0xB4] = { "LDY", &CPU6502::LDY, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0xB5] = { "LDA", &CPU6502::LDA, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0xB6] = { "LDX", &CPU6502::LDX, &CPU6502::ZPY, AddrMode::ZPY, 4 };
    _instructions[0xB7] = { "LAX", &CPU6502::LAX, &CPU6502::ZPY, AddrMode::ZPY, 4 };
    _instructions[0xB8] = { "CLV", &CPU6502::CLV, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xB9] = { "LDA", &CPU6502::LDA, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0xBA] = { "TSX", &CPU6502::TSX, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xBB] = { "LAS", &CPU6502::LAS, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0xBC] = { "LDY", &CPU6502::LDY, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0xBD] = { "LDA", &CPU6502::LDA, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0xBE] = { "LDX", &CPU6502::LDX, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0xBF] = { "LAX", &CPU6502::LAX, &CPU6502::ABY, AddrMode::ABY, 4 };

    //' $C0-$CF
    _instructions[0xC0] = { "CPY", &CPU6502::CPY, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xC1] = { "CMP", &CPU6502::CMP, &CPU6502::IZX, AddrMode::IZX, 6 };
    _instructions[0xC2] = { "NOP", &CPU6502::NOP, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xC3] = { "DCP", &CPU6502::DCP, &CPU6502::IZX, AddrMode::IZX, 8 };
    _instructions[0xC4] = { "CPY", &CPU6502::CPY, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0xC5] = { "CMP", &CPU6502::CMP, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0xC6] = { "DEC", &CPU6502::DEC, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0xC7] = { "DCP", &CPU6502::DCP, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0xC8] = { "INY", &CPU6502::INY, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xC9] = { "CMP", &CPU6502::CMP, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xCA] = { "DEX", &CPU6502::DEX, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xCB] = { "AXS", &CPU6502::AXS, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xCC] = { "CPY", &CPU6502::CPY, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0xCD] = { "CMP", &CPU6502::CMP, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0xCE] = { "DEC", &CPU6502::DEC, &CPU6502::ABS, AddrMode::ABS, 6 };
    _instructions[0xCF] = { "DCP", &CPU6502::DCP, &CPU6502::ABS, AddrMode::ABS, 6 };

    //' $D0-$DF
    _instructions[0xD0] = { "BNE", &CPU6502::BNE, &CPU6502::REL, AddrMode::REL, 2 };
    _instructions[0xD1] = { "CMP", &CPU6502::CMP, &CPU6502::IZY, AddrMode::IZY, 5 };

    _instructions[0xD3] = { "DCP", &CPU6502::DCP, &CPU6502::IZY, AddrMode::IZY, 8 };
    _instructions[0xD4] = { "NOP", &CPU6502::NOP, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0xD5] = { "CMP", &CPU6502::CMP, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0xD6] = { "DEC", &CPU6502::DEC, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0xD7] = { "DCP", &CPU6502::DCP, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0xD8] = { "CLD", &CPU6502::CLD, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xD9] = { "CMP", &CPU6502::CMP, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0xDA] = { "NOP", &CPU6502::NOP, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xDB] = { "DCP", &CPU6502::DCP, &CPU6502::ABY, AddrMode::ABY, 7 };
    _instructions[0xDC] = { "NOP", &CPU6502::NOP, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0xDD] = { "CMP", &CPU6502::CMP, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0xDE] = { "DEC", &CPU6502::DEC, &CPU6502::ABX, AddrMode::ABX, 7 };
    _instructions[0xDF] = { "DCP", &CPU6502::DCP, &CPU6502::ABX, AddrMode::ABX, 7 };

    //' $E0-$EF
    _instructions[0xE0] = { "CPX", &CPU6502::CPX, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xE1] = { "SBC", &CPU6502::SBC, &CPU6502::IZX, AddrMode::IZX, 6 };
    _instructions[0xE2] = { "NOP", &CPU6502::NOP, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xE3] = { "ISB", &CPU6502::ISB, &CPU6502::IZX, AddrMode::IZX, 8 };
    _instructions[0xE4] = { "CPX", &CPU6502::CPX, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0xE5] = { "SBC", &CPU6502::SBC, &CPU6502::ZP0, AddrMode::ZP0, 3 };
    _instructions[0xE6] = { "INC", &CPU6502::INC, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0xE7] = { "ISB", &CPU6502::ISB, &CPU6502::ZP0, AddrMode::ZP0, 5 };
    _instructions[0xE8] = { "INX", &CPU6502::INX, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xE9] = { "SBC", &CPU6502::SBC, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xEA] = { "NOP", &CPU6502::NOP, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xEB] = { "SBC", &CPU6502::SBC, &CPU6502::IMM, AddrMode::IMM, 2 };
    _instructions[0xEC] = { "CPX", &CPU6502::CPX, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0xED] = { "SBC", &CPU6502::SBC, &CPU6502::ABS, AddrMode::ABS, 4 };
    _instructions[0xEE] = { "INC", &CPU6502::INC, &CPU6502::ABS, AddrMode::ABS, 6 };
    _instructions[0xEF] = { "ISB", &CPU6502::ISB, &CPU6502::ABS, AddrMode::ABS, 6 };

    // $F0-$FF
    _instructions[0xF0] = { "BEQ", &CPU6502::BEQ, &CPU6502::REL, AddrMode::REL, 2 };
    _instructions[0xF1] = { "SBC", &CPU6502::SBC, &CPU6502::IZY, AddrMode::IZY, 5 };

    _instructions[0xF3] = { "ISB", &CPU6502::ISB, &CPU6502::IZY, AddrMode::IZY, 8 };
    _instructions[0xF4] = { "NOP", &CPU6502::NOP, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0xF5] = { "SBC", &CPU6502::SBC, &CPU6502::ZPX, AddrMode::ZPX, 4 };
    _instructions[0xF6] = { "INC", &CPU6502::INC, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0xF7] = { "ISB", &CPU6502::ISB, &CPU6502::ZPX, AddrMode::ZPX, 6 };
    _instructions[0xF8] = { "SED", &CPU6502::SED, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xF9] = { "SBC", &CPU6502::SBC, &CPU6502::ABY, AddrMode::ABY, 4 };
    _instructions[0xFA] = { "NOP", &CPU6502::NOP, &CPU6502::IMP, AddrMode::IMP, 2 };
    _instructions[0xFB] = { "ISB", &CPU6502::ISB, &CPU6502::ABY, AddrMode::ABY, 7 };
    _instructions[0xFC] = { "NOP", &CPU6502::NOP, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0xFD] = { "SBC", &CPU6502::SBC, &CPU6502::ABX, AddrMode::ABX, 4 };
    _instructions[0xFE] = { "INC", &CPU6502::INC, &CPU6502::ABX, AddrMode::ABX, 7 };
    _instructions[0xFF] = { "ISB", &CPU6502::ISB, &CPU6502::ABX, AddrMode::ABX, 7 };
}
