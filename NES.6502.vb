Imports System.Security.Cryptography
Imports System.Windows.Forms.VisualStyles.VisualStyleElement

Namespace NintendoEntertainmentSystem

    Public Class em6502
        'Public OpcodeHandlers As New Hashtable
        Delegate Function OpCodeDelegation() As Byte
        Delegate Function AddressModeDelegation() As Byte
        'these can both be set as a single delegate function

        <Flags>
        Public Enum FLAGS6502
            C = (1 << 0) 'Carry Bit
            Z = (1 << 1) 'Zero
            I = (1 << 2) 'Disable Interupts
            D = (1 << 3) 'Decimal Mode (unused in this implementation)
            B = (1 << 4) 'Break
            U = (1 << 5) 'Unused
            V = (1 << 6) 'Overflow
            N = (1 << 7) 'Negative
        End Enum

#Region "Registers"
        Public A As Byte = &H0              'Accumulator Register
        Public X As Byte = &H0              'X Register
        Public Y As Byte = &H0              'Y Register
        Public StackPointer As Byte = &H0   'Stack Location on the bus
        Public PC As UInt16 = &H0           'Program Counter
        Public Status As Byte = &H0         'Status Register

        Private Sub DecrementStackpointer()
            If StackPointer = 0 Then
                StackPointer = &HFFUI
            Else
                StackPointer -= 1
            End If
        End Sub
        Private Sub IncrementStackpointer()
            If StackPointer = &HFFUI Then
                StackPointer = 0
            Else
                StackPointer += 1
            End If
        End Sub
        Private Sub DecrementProgramCounter()
            If PC = 0 Then
                PC = &HFFFFUS
            Else
                PC -= 1
            End If
        End Sub
        Private Sub IncrementProgramCounter()
            If PC = &HFFFFUS Then
                PC = 0
            Else
                PC += 1
            End If
        End Sub
        Private Sub IncrementCycles()
            If cycles = &HFFUS Then
                cycles = 0
            Else
                cycles += 1
            End If
        End Sub

#End Region

#Region "Instruction Array"
        Private Structure INSTRUCTIONS
            Public name As String
            Public Operate As OpCodeDelegation
            Public AddrMode As AddressModeDelegation
            Public cycles As Byte
        End Structure
        Private Const TOTAL_INSTRUCTIONS As Integer = 255 'VB
        Private lookup(TOTAL_INSTRUCTIONS) As INSTRUCTIONS
#End Region

        Public InstructionCount As UInteger = 0

#Region "Construction - Destruction"
        Private Sub InitializeHandler(index As Integer, name As String, Fun1 As OpCodeDelegation, Fun2 As AddressModeDelegation, cycles As Integer)
            With lookup(index)
                .name = name
                .Operate = Fun1
                .AddrMode = Fun2
                .cycles = cycles
            End With
        End Sub
        Public Sub New()
            InitializeHandler(&H0, "BRK", AddressOf BRK, AddressOf IMM, 7)
            InitializeHandler(&H1, "ORA", AddressOf ORA, AddressOf IZX, 6)
            InitializeHandler(&H2, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H3, "SLO", AddressOf SLO, AddressOf IZX, 8) ' ASL then ORA
            InitializeHandler(&H4, "NOP", AddressOf NOP, AddressOf ZP0, 3)
            InitializeHandler(&H5, "ORA", AddressOf ORA, AddressOf ZP0, 3)
            InitializeHandler(&H6, "ASL", AddressOf ASL, AddressOf ZP0, 5)
            InitializeHandler(&H7, "SLO", AddressOf SLO, AddressOf ZP0, 5)
            InitializeHandler(&H8, "PHP", AddressOf PHP, AddressOf IMP, 3)
            InitializeHandler(&H9, "ORA", AddressOf ORA, AddressOf IMM, 2)
            InitializeHandler(&HA, "ASL", AddressOf ASL, AddressOf IMP, 2)
            InitializeHandler(&HB, "ANC", AddressOf ANC, AddressOf IMM, 2)
            InitializeHandler(&HC, "NOP", AddressOf NOP, AddressOf ABS, 4)
            InitializeHandler(&HD, "ORA", AddressOf ORA, AddressOf ABS, 4)
            InitializeHandler(&HE, "ASL", AddressOf ASL, AddressOf ABS, 6)
            InitializeHandler(&HF, "SLO", AddressOf SLO, AddressOf ABS, 6)

            InitializeHandler(&H10, "BPL", AddressOf BPL, AddressOf REL, 2)
            InitializeHandler(&H11, "ORA", AddressOf ORA, AddressOf IZY, 5)
            InitializeHandler(&H12, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H13, "SLO", AddressOf SLO, AddressOf IZY, 8)
            InitializeHandler(&H14, "NOP", AddressOf NOP, AddressOf ZPX, 4)
            InitializeHandler(&H15, "ORA", AddressOf ORA, AddressOf ZPX, 4)
            InitializeHandler(&H16, "ASL", AddressOf ASL, AddressOf ZPX, 6)
            InitializeHandler(&H17, "SLO", AddressOf SLO, AddressOf ZPX, 6)
            InitializeHandler(&H18, "CLC", AddressOf CLC, AddressOf IMP, 2)
            InitializeHandler(&H19, "ORA", AddressOf ORA, AddressOf ABY, 4)
            InitializeHandler(&H1A, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(&H1B, "SLO", AddressOf SLO, AddressOf ABY, 7)
            InitializeHandler(&H1C, "NOP", AddressOf NOP, AddressOf ABX, 4)
            InitializeHandler(&H1D, "ORA", AddressOf ORA, AddressOf ABX, 4)
            InitializeHandler(&H1E, "ASL", AddressOf ASL, AddressOf ABX, 7)
            InitializeHandler(&H1F, "SLO", AddressOf SLO, AddressOf ABX, 7)

            InitializeHandler(&H20, "JSR", AddressOf JSR, AddressOf ABS, 6)
            InitializeHandler(&H21, "AND", AddressOf AND_, AddressOf IZX, 6)
            InitializeHandler(&H22, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H23, "RLA", AddressOf RLA, AddressOf IZX, 8) ' ROL then AND
            InitializeHandler(&H24, "BIT", AddressOf BIT, AddressOf ZP0, 3)
            InitializeHandler(&H25, "AND", AddressOf AND_, AddressOf ZP0, 3)
            InitializeHandler(&H26, "ROL", AddressOf ROL, AddressOf ZP0, 5)
            InitializeHandler(&H27, "RLA", AddressOf RLA, AddressOf ZP0, 5)
            InitializeHandler(&H28, "PLP", AddressOf PLP, AddressOf IMP, 4)
            InitializeHandler(&H29, "AND", AddressOf AND_, AddressOf IMM, 2)
            InitializeHandler(&H2A, "ROL", AddressOf ROL, AddressOf IMP, 2)
            InitializeHandler(&H2B, "ANC", AddressOf ANC, AddressOf IMM, 2)
            InitializeHandler(&H2C, "BIT", AddressOf BIT, AddressOf ABS, 4)
            InitializeHandler(&H2D, "AND", AddressOf AND_, AddressOf ABS, 4)
            InitializeHandler(&H2E, "ROL", AddressOf ROL, AddressOf ABS, 6)
            InitializeHandler(&H2F, "RLA", AddressOf RLA, AddressOf ABS, 6)

            InitializeHandler(&H30, "BMI", AddressOf BMI, AddressOf REL, 2)
            InitializeHandler(&H31, "AND", AddressOf AND_, AddressOf IZY, 5)
            InitializeHandler(&H32, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H33, "RLA", AddressOf RLA, AddressOf IZY, 8)
            InitializeHandler(&H34, "NOP", AddressOf NOP, AddressOf ZPX, 4)
            InitializeHandler(&H35, "AND", AddressOf AND_, AddressOf ZPX, 4)
            InitializeHandler(&H36, "ROL", AddressOf ROL, AddressOf ZPX, 6)
            InitializeHandler(&H37, "RLA", AddressOf RLA, AddressOf ZPX, 6)
            InitializeHandler(&H38, "SEC", AddressOf SEC, AddressOf IMP, 2)
            InitializeHandler(&H39, "AND", AddressOf AND_, AddressOf ABY, 4)
            InitializeHandler(&H3A, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(&H3B, "RLA", AddressOf RLA, AddressOf ABY, 7)
            InitializeHandler(&H3C, "NOP", AddressOf NOP, AddressOf ABX, 4)
            InitializeHandler(&H3D, "AND", AddressOf AND_, AddressOf ABX, 4)
            InitializeHandler(&H3E, "ROL", AddressOf ROL, AddressOf ABX, 7)
            InitializeHandler(&H3F, "RLA", AddressOf RLA, AddressOf ABX, 7)

            InitializeHandler(&H40, "RTI", AddressOf RTI, AddressOf IMP, 6)
            InitializeHandler(&H41, "EOR", AddressOf EOR, AddressOf IZX, 6)
            InitializeHandler(&H42, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H43, "SRE", AddressOf SRE, AddressOf IZX, 8) ' LSR then EOR
            InitializeHandler(&H44, "NOP", AddressOf NOP, AddressOf ZP0, 3)
            InitializeHandler(&H45, "EOR", AddressOf EOR, AddressOf ZP0, 3)
            InitializeHandler(&H46, "LSR", AddressOf LSR, AddressOf ZP0, 5)
            InitializeHandler(&H47, "SRE", AddressOf SRE, AddressOf ZP0, 5)
            InitializeHandler(&H48, "PHA", AddressOf PHA, AddressOf IMP, 3)
            InitializeHandler(&H49, "EOR", AddressOf EOR, AddressOf IMM, 2)
            InitializeHandler(&H4A, "LSR", AddressOf LSR, AddressOf IMP, 2)
            InitializeHandler(&H4B, "ALR", AddressOf ALR, AddressOf IMM, 2)
            InitializeHandler(&H4C, "JMP", AddressOf JMP, AddressOf ABS, 3)
            InitializeHandler(&H4D, "EOR", AddressOf EOR, AddressOf ABS, 4)
            InitializeHandler(&H4E, "LSR", AddressOf LSR, AddressOf ABS, 6)
            InitializeHandler(&H4F, "SRE", AddressOf SRE, AddressOf ABS, 6)

            InitializeHandler(&H50, "BVC", AddressOf BVC, AddressOf REL, 2)
            InitializeHandler(&H51, "EOR", AddressOf EOR, AddressOf IZY, 5)
            InitializeHandler(&H52, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H53, "SRE", AddressOf SRE, AddressOf IZY, 8)
            InitializeHandler(&H54, "NOP", AddressOf NOP, AddressOf ZPX, 4)
            InitializeHandler(&H55, "EOR", AddressOf EOR, AddressOf ZPX, 4)
            InitializeHandler(&H56, "LSR", AddressOf LSR, AddressOf ZPX, 6)
            InitializeHandler(&H57, "SRE", AddressOf SRE, AddressOf ZPX, 6)
            InitializeHandler(&H58, "CLI", AddressOf CLI, AddressOf IMP, 2)
            InitializeHandler(&H59, "EOR", AddressOf EOR, AddressOf ABY, 4)
            InitializeHandler(&H5A, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(&H5B, "SRE", AddressOf SRE, AddressOf ABY, 7)
            InitializeHandler(&H5C, "NOP", AddressOf NOP, AddressOf ABX, 4)
            InitializeHandler(&H5D, "EOR", AddressOf EOR, AddressOf ABX, 4)
            InitializeHandler(&H5E, "LSR", AddressOf LSR, AddressOf ABX, 7)
            InitializeHandler(&H5F, "SRE", AddressOf SRE, AddressOf ABX, 7)

            InitializeHandler(&H60, "RTS", AddressOf RTS, AddressOf IMP, 6)
            InitializeHandler(&H61, "ADC", AddressOf ADC, AddressOf IZX, 6)
            InitializeHandler(&H62, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H63, "RRA", AddressOf RRA, AddressOf IZX, 8) ' ROR then ADC
            InitializeHandler(&H64, "NOP", AddressOf NOP, AddressOf ZP0, 3)
            InitializeHandler(&H65, "ADC", AddressOf ADC, AddressOf ZP0, 3)
            InitializeHandler(&H66, "ROR", AddressOf ROR, AddressOf ZP0, 5)
            InitializeHandler(&H67, "RRA", AddressOf RRA, AddressOf ZP0, 5)
            InitializeHandler(&H68, "PLA", AddressOf PLA, AddressOf IMP, 4)
            InitializeHandler(&H69, "ADC", AddressOf ADC, AddressOf IMM, 2)
            InitializeHandler(&H6A, "ROR", AddressOf ROR, AddressOf IMP, 2)
            InitializeHandler(&H6B, "ARR", AddressOf ARR, AddressOf IMM, 2)
            InitializeHandler(&H6C, "JMP", AddressOf JMP, AddressOf IND, 5)
            InitializeHandler(&H6D, "ADC", AddressOf ADC, AddressOf ABS, 4)
            InitializeHandler(&H6E, "ROR", AddressOf ROR, AddressOf ABS, 6)
            InitializeHandler(&H6F, "RRA", AddressOf RRA, AddressOf ABS, 6)

            InitializeHandler(&H70, "BVS", AddressOf BVS, AddressOf REL, 2)
            InitializeHandler(&H71, "ADC", AddressOf ADC, AddressOf IZY, 5)
            InitializeHandler(&H72, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H73, "RRA", AddressOf RRA, AddressOf IZY, 8)
            InitializeHandler(&H74, "NOP", AddressOf NOP, AddressOf ZPX, 4)
            InitializeHandler(&H75, "ADC", AddressOf ADC, AddressOf ZPX, 4)
            InitializeHandler(&H76, "ROR", AddressOf ROR, AddressOf ZPX, 6)
            InitializeHandler(&H77, "RRA", AddressOf RRA, AddressOf ZPX, 6)
            InitializeHandler(&H78, "SEI", AddressOf SEI, AddressOf IMP, 2)
            InitializeHandler(&H79, "ADC", AddressOf ADC, AddressOf ABY, 4)
            InitializeHandler(&H7A, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(&H7B, "RRA", AddressOf RRA, AddressOf ABY, 7)
            InitializeHandler(&H7C, "NOP", AddressOf NOP, AddressOf ABX, 4)
            InitializeHandler(&H7D, "ADC", AddressOf ADC, AddressOf ABX, 4)
            InitializeHandler(&H7E, "ROR", AddressOf ROR, AddressOf ABX, 7)
            InitializeHandler(&H7F, "RRA", AddressOf RRA, AddressOf ABX, 7)

            InitializeHandler(&H80, "NOP", AddressOf NOP, AddressOf IMM, 2)
            InitializeHandler(&H81, "STA", AddressOf STA, AddressOf IZX, 6)
            InitializeHandler(&H82, "NOP", AddressOf NOP, AddressOf IMM, 2)
            InitializeHandler(&H83, "SAX", AddressOf SAX, AddressOf IZX, 6) ' Store A AND X
            InitializeHandler(&H84, "STY", AddressOf STY, AddressOf ZP0, 3)
            InitializeHandler(&H85, "STA", AddressOf STA, AddressOf ZP0, 3)
            InitializeHandler(&H86, "STX", AddressOf STX, AddressOf ZP0, 3)
            InitializeHandler(&H87, "SAX", AddressOf SAX, AddressOf ZP0, 3)
            InitializeHandler(&H88, "DEY", AddressOf DEY, AddressOf IMP, 2)
            InitializeHandler(&H89, "NOP", AddressOf NOP, AddressOf IMM, 2)
            InitializeHandler(&H8A, "TXA", AddressOf TXA, AddressOf IMP, 2)
            InitializeHandler(&H8B, "XAA", AddressOf XAA, AddressOf IMM, 2)
            InitializeHandler(&H8C, "STY", AddressOf STY, AddressOf ABS, 4)
            InitializeHandler(&H8D, "STA", AddressOf STA, AddressOf ABS, 4)
            InitializeHandler(&H8E, "STX", AddressOf STX, AddressOf ABS, 4)
            InitializeHandler(&H8F, "SAX", AddressOf SAX, AddressOf ABS, 4)

            InitializeHandler(&H90, "BCC", AddressOf BCC, AddressOf REL, 2)
            InitializeHandler(&H91, "STA", AddressOf STA, AddressOf IZY, 6)
            InitializeHandler(&H92, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&H93, "SHA", AddressOf SHA, AddressOf IZY, 6)
            InitializeHandler(&H94, "STY", AddressOf STY, AddressOf ZPX, 4)
            InitializeHandler(&H95, "STA", AddressOf STA, AddressOf ZPX, 4)
            InitializeHandler(&H96, "STX", AddressOf STX, AddressOf ZPY, 4)
            InitializeHandler(&H97, "SAX", AddressOf SAX, AddressOf ZPY, 4)
            InitializeHandler(&H98, "TYA", AddressOf TYA, AddressOf IMP, 2)
            InitializeHandler(&H99, "STA", AddressOf STA, AddressOf ABY, 5)
            InitializeHandler(&H9A, "TXS", AddressOf TXS, AddressOf IMP, 2)
            InitializeHandler(&H9B, "TAS", AddressOf TAS, AddressOf ABY, 5)
            InitializeHandler(&H9C, "SHY", AddressOf SHY, AddressOf ABX, 5)
            InitializeHandler(&H9D, "STA", AddressOf STA, AddressOf ABX, 5)
            InitializeHandler(&H9E, "SHX", AddressOf SHX, AddressOf ABY, 5)
            InitializeHandler(&H9F, "SHA", AddressOf SHA, AddressOf ABY, 5)

            InitializeHandler(&HA0, "LDY", AddressOf LDY, AddressOf IMM, 2)
            InitializeHandler(&HA1, "LDA", AddressOf LDA, AddressOf IZX, 6)
            InitializeHandler(&HA2, "LDX", AddressOf LDX, AddressOf IMM, 2)
            InitializeHandler(&HA3, "LAX", AddressOf LAX, AddressOf IZX, 6) ' Load A and X
            InitializeHandler(&HA4, "LDY", AddressOf LDY, AddressOf ZP0, 3)
            InitializeHandler(&HA5, "LDA", AddressOf LDA, AddressOf ZP0, 3)
            InitializeHandler(&HA6, "LDX", AddressOf LDX, AddressOf ZP0, 3)
            InitializeHandler(&HA7, "LAX", AddressOf LAX, AddressOf ZP0, 3)
            InitializeHandler(&HA8, "TAY", AddressOf TAY, AddressOf IMP, 2)
            InitializeHandler(&HA9, "LDA", AddressOf LDA, AddressOf IMM, 2)
            InitializeHandler(&HAA, "TAX", AddressOf TAX, AddressOf IMP, 2)
            InitializeHandler(&HAB, "ARR", AddressOf ARR, AddressOf IMM, 2)
            InitializeHandler(&HAC, "LDY", AddressOf LDY, AddressOf ABS, 4)
            InitializeHandler(&HAD, "LDA", AddressOf LDA, AddressOf ABS, 4)
            InitializeHandler(&HAE, "LDX", AddressOf LDX, AddressOf ABS, 4)
            InitializeHandler(&HAF, "LAX", AddressOf LAX, AddressOf ABS, 4)

            InitializeHandler(&HB0, "BCS", AddressOf BCS, AddressOf REL, 2)
            InitializeHandler(&HB1, "LDA", AddressOf LDA, AddressOf IZY, 5)
            InitializeHandler(&HB2, "KIL", AddressOf KIL, AddressOf IMP, 0) ' 2
            InitializeHandler(&HB3, "LAX", AddressOf LAX, AddressOf IZY, 5) ' (+1 if crosses boundry)
            InitializeHandler(&HB4, "LDY", AddressOf LDY, AddressOf ZPX, 4)
            InitializeHandler(&HB5, "LDA", AddressOf LDA, AddressOf ZPX, 4)
            InitializeHandler(&HB6, "LDX", AddressOf LDX, AddressOf ZPY, 4)
            InitializeHandler(&HB7, "LAX", AddressOf LAX, AddressOf ZPY, 4)
            InitializeHandler(&HB8, "CLV", AddressOf CLV, AddressOf IMP, 2)
            InitializeHandler(&HB9, "LDA", AddressOf LDA, AddressOf ABY, 4)
            InitializeHandler(&HBA, "TSX", AddressOf TSX, AddressOf IMP, 2)
            InitializeHandler(&HBB, "LAS", AddressOf LAS, AddressOf ABY, 4)
            InitializeHandler(&HBC, "LDY", AddressOf LDY, AddressOf ABX, 4)
            InitializeHandler(&HBD, "LDA", AddressOf LDA, AddressOf ABX, 4)
            InitializeHandler(&HBE, "LDX", AddressOf LDX, AddressOf ABY, 4)
            InitializeHandler(&HBF, "LAX", AddressOf LAX, AddressOf ABY, 4) 'LAX BF 3 bytes 4 cycles (+1 if crosses boundry)

            InitializeHandler(&HC0, "CPY", AddressOf CPY, AddressOf IMM, 2)
            InitializeHandler(&HC1, "CMP", AddressOf CMP, AddressOf IZX, 6)
            InitializeHandler(&HC2, "NOP", AddressOf NOP, AddressOf IMM, 2)
            InitializeHandler(&HC3, "DCP", AddressOf DCP, AddressOf IZX, 8)
            InitializeHandler(&HC4, "CPY", AddressOf CPY, AddressOf ZP0, 3)
            InitializeHandler(&HC5, "CMP", AddressOf CMP, AddressOf ZP0, 3)
            InitializeHandler(&HC6, "DEC", AddressOf DEC, AddressOf ZP0, 5)
            InitializeHandler(&HC7, "DCP", AddressOf DCP, AddressOf ZP0, 5)
            InitializeHandler(&HC8, "INY", AddressOf INY, AddressOf IMP, 2)
            InitializeHandler(&HC9, "CMP", AddressOf CMP, AddressOf IMM, 2)
            InitializeHandler(&HCA, "DEX", AddressOf DEX, AddressOf IMP, 2)
            InitializeHandler(&HCB, "AXS", AddressOf AXS, AddressOf IMM, 2)
            InitializeHandler(&HCC, "CPY", AddressOf CPY, AddressOf ABS, 4)
            InitializeHandler(&HCD, "CMP", AddressOf CMP, AddressOf ABS, 4)
            InitializeHandler(&HCE, "DEC", AddressOf DEC, AddressOf ABS, 6)
            InitializeHandler(&HCF, "DCP", AddressOf DCP, AddressOf ABS, 6)

            InitializeHandler(&HD0, "BNE", AddressOf BNE, AddressOf REL, 2)
            InitializeHandler(&HD1, "CMP", AddressOf CMP, AddressOf IZY, 5)
            InitializeHandler(&HD2, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&HD3, "DCP", AddressOf DCP, AddressOf IZY, 8)
            InitializeHandler(&HD4, "NOP", AddressOf NOP, AddressOf ZPX, 4)
            InitializeHandler(&HD5, "CMP", AddressOf CMP, AddressOf ZPX, 4)
            InitializeHandler(&HD6, "DEC", AddressOf DEC, AddressOf ZPX, 6)
            InitializeHandler(&HD7, "DCP", AddressOf DCP, AddressOf ZPX, 6)
            InitializeHandler(&HD8, "CLD", AddressOf CLD, AddressOf IMP, 2)
            InitializeHandler(&HD9, "CMP", AddressOf CMP, AddressOf ABY, 4)
            InitializeHandler(&HDA, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(&HDB, "DCP", AddressOf DCP, AddressOf ABY, 7)
            InitializeHandler(&HDC, "NOP", AddressOf NOP, AddressOf ABX, 4)
            InitializeHandler(&HDD, "CMP", AddressOf CMP, AddressOf ABX, 4)
            InitializeHandler(&HDE, "DEC", AddressOf DEC, AddressOf ABX, 7)
            InitializeHandler(&HDF, "DCP", AddressOf DCP, AddressOf ABX, 7)

            InitializeHandler(&HE0, "CPX", AddressOf CPX, AddressOf IMM, 2)
            InitializeHandler(&HE1, "SBC", AddressOf SBC, AddressOf IZX, 6)
            InitializeHandler(&HE2, "NOP", AddressOf NOP, AddressOf IMM, 2)
            InitializeHandler(&HE3, "ISB", AddressOf ISB, AddressOf IZX, 8)
            InitializeHandler(&HE4, "CPX", AddressOf CPX, AddressOf ZP0, 3)
            InitializeHandler(&HE5, "SBC", AddressOf SBC, AddressOf ZP0, 3)
            InitializeHandler(&HE6, "INC", AddressOf INC, AddressOf ZP0, 5)
            InitializeHandler(&HE7, "ISB", AddressOf ISB, AddressOf ZP0, 5)
            InitializeHandler(&HE8, "INX", AddressOf INX, AddressOf IMP, 2)
            InitializeHandler(&HE9, "SBC", AddressOf SBC, AddressOf IMM, 2)
            InitializeHandler(&HEA, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(&HEB, "SBC", AddressOf SBC, AddressOf IMM, 2)
            InitializeHandler(&HEC, "CPX", AddressOf CPX, AddressOf ABS, 4)
            InitializeHandler(&HED, "SBC", AddressOf SBC, AddressOf ABS, 4)
            InitializeHandler(&HEE, "INC", AddressOf INC, AddressOf ABS, 6)
            InitializeHandler(&HEF, "ISB", AddressOf ISB, AddressOf ABS, 6)

            InitializeHandler(&HF0, "BEQ", AddressOf BEQ, AddressOf REL, 2)
            InitializeHandler(&HF1, "SBC", AddressOf SBC, AddressOf IZY, 5)
            InitializeHandler(&HF2, "KIL", AddressOf KIL, AddressOf IMP, 0) '2
            InitializeHandler(&HF3, "ISB", AddressOf ISB, AddressOf IZY, 8)
            InitializeHandler(&HF4, "NOP", AddressOf NOP, AddressOf ZPX, 4)
            InitializeHandler(&HF5, "SBC", AddressOf SBC, AddressOf ZPX, 4)
            InitializeHandler(&HF6, "INC", AddressOf INC, AddressOf ZPX, 6)
            InitializeHandler(&HF7, "ISB", AddressOf ISB, AddressOf ZPX, 6)
            InitializeHandler(&HF8, "SED", AddressOf SED, AddressOf IMP, 2)
            InitializeHandler(&HF9, "SBC", AddressOf SBC, AddressOf ABY, 4)
            InitializeHandler(&HFA, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(&HFB, "ISB", AddressOf ISB, AddressOf ABY, 7)
            InitializeHandler(&HFC, "NOP", AddressOf NOP, AddressOf ABX, 4)
            InitializeHandler(&HFD, "SBC", AddressOf SBC, AddressOf ABX, 4)
            InitializeHandler(&HFE, "INC", AddressOf INC, AddressOf ABX, 7)
            InitializeHandler(&HFF, "ISB", AddressOf ISB, AddressOf ABX, 7)
        End Sub

        ' masswerk.at/6502/6502_instruction_set.html (another time perhaps)
        ' un official op codes listed in the NESTEST cartridge
        ' NOP (opcode EA)
        ' LAX (opcodes noted above) 
        ' SAX (opcode noted above)
        ' SBC (opcode EB)
        ' DCP (opcode ??)
        ' ISB (opcode ??)
        ' SLO (opcode ??)
        ' RLA (opcode ??)
        ' SRE (opcode ??)
        ' RRA (opcode ??)

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub
#End Region

#Region "RAM BUS"
        Private Shared Property Bus() As NESBus
        Public Sub ConnectBus(ByRef RamBus As NESBus)
            Bus = RamBus
        End Sub

#End Region

#Region "Addressing Modes [check]"
        Public Function IMP() As Byte
            fetched = A
            Return &H0
        End Function

        Public Function IMM() As Byte
            addr_abs = PC
            IncrementProgramCounter()
            Return &H0
        End Function

        Public Function ZP0() As Byte
            addr_abs = Read(PC)
            IncrementProgramCounter()
            addr_abs = addr_abs And &HFFUS
            Return 0
        End Function

        Public Function ZPX() As Byte
            'addr_abs = (Read(PC)) + X
            addr_abs = MathHelpers.SafeAddition16(Read(PC), X)
            IncrementProgramCounter()
            addr_abs = addr_abs And &HFFUS
            Return &H0
        End Function

        Public Function ZPY() As Byte
            'addr_abs = (Read(PC)) + Y
            addr_abs = MathHelpers.SafeAddition16(Read(PC), Y)
            IncrementProgramCounter()
            addr_abs = addr_abs And &HFFUS
            Return &H0
        End Function

        Public Function REL() As Byte
            addr_rel = Read(PC)
            IncrementProgramCounter()
            If addr_rel And &H80US Then
                addr_rel = addr_rel Or &HFF00US
            End If
            Return &H0
        End Function

        Public Function ABS() As Byte
            Dim lo As Byte = Read(PC)
            IncrementProgramCounter()
            Dim hi As Byte = Read(PC)
            IncrementProgramCounter()
            addr_abs = (CUShort(hi) << 8) Or CUShort(lo)
            Return &H0
        End Function

        Public Function ABX() As Byte
            Dim lo As Byte = Read(PC)
            IncrementProgramCounter()
            Dim hi As Byte = Read(PC)
            IncrementProgramCounter()

            addr_abs = (CUShort(hi) << 8) Or CUShort(lo)
            addr_abs += X
            'addr_abs = MathHelpers.SafeAddition16((hi << 8) Or lo, X)

            If (addr_abs And &HFF00US) <> (CUShort(hi) << 8) Then
                Return 1
            Else
                Return 0
            End If
        End Function

        Public Function ABY() As Byte
            Dim lo As Byte = Read(PC)
            IncrementProgramCounter()
            Dim hi As Byte = Read(PC)
            IncrementProgramCounter()

            addr_abs = (CUShort(hi) << 8) Or CUShort(lo)
            addr_abs += Y
            'addr_abs = MathHelpers.SafeAddition16((hi << 8) Or lo, Y)

            If (addr_abs And &HFF00US) <> (CUShort(hi) << 8) Then
                Return 1
            Else
                Return 0
            End If
        End Function

        Public Function IND() As Byte
            Dim ptr_lo As Byte = Read(PC)
            IncrementProgramCounter()
            Dim ptr_hi As Byte = Read(PC)
            IncrementProgramCounter()

            Dim ptr As UShort = (CUShort(ptr_hi) << 8) Or CUShort(ptr_lo)

            If ptr_lo = CByte(&HFFUI) Then
                addr_abs = MathHelpers.SafeOr16(MathHelpers.SafeShiftLeft16(Read(ptr And &HFF00US), 8), Read(ptr + 0))
            Else
                addr_abs = MathHelpers.SafeOr16(MathHelpers.SafeShiftLeft16(Read(MathHelpers.SafeAddition16(ptr, 1)), 8), Read(ptr + 0))
            End If
            Return 0
        End Function

        Public Function IZX() As Byte
            Dim t As UInt16 = Read(PC)
            IncrementProgramCounter()

            Dim lo As Byte = Read((MathHelpers.SafeAddition16(t, X)) And &HFFUS)
            Dim hi As Byte = Read((MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(t, X), 1)) And &HFFUS)
            addr_abs = (CUShort(hi) << 8) Or CUShort(lo)

            Return 0
        End Function

        Public Function IZY() As Byte
            Dim t As UInt16 = Read(PC)
            IncrementProgramCounter()

            Dim lo As Byte = Read(t And &HFFUS)
            Dim hi As Byte = Read((MathHelpers.SafeAddition16(t, 1)) And &HFFUS)

            addr_abs = (CUShort(hi) << 8) Or CUShort(lo)
            addr_abs = MathHelpers.SafeAddition16(addr_abs, Y)
            'addr_abs = MathHelpers.SafeAddition16(MathHelpers.SafeShiftLeft16(hi, 8) Or lo, Y)

            If (addr_abs And &HFF00US) <> (CUShort(hi) << 8) Then
                Return 1
            Else
                Return 0
            End If
        End Function

        Private Function MatchParentDelegate(a As Object, b As AddressModeDelegation) As Boolean
            If a = b Then
                Return True
            End If
            Return False
        End Function

        Public Function Fetch() As Byte
            If MatchParentDelegate(lookup(opcode).AddrMode, AddressOf Me.IMP) = False Then
                fetched = Read(addr_abs)
            End If
            Return fetched
        End Function

        Public Function ADC() As Byte
            'data were adding to the accumulator
            Fetch()

            'add is performed in 16bit domain for emulation for emulation to capture any
            'carry bit, which will exist in bit 8 of the 16bit word
            'temp = A + fetched + GetFlag(FLAGS6502.C)

            'Because VB hates you
            temp = MathHelpers.SafeAddition16(A, fetched)
            temp = MathHelpers.SafeAddition16(temp, GetFlag(FLAGS6502.C))

            'the carry flag out exists in the high byte bit 0
            SetFlag(FLAGS6502.C, temp > 255)

            'The zero flag is set if the result is 0
            SetFlag(FLAGS6502.Z, (temp And &HFFUS) = 0)

            'the signed overflow flag is set based on all that up there
            SetFlag(FLAGS6502.V, (Not (A Xor fetched) And (A Xor temp)) And &H80US) 'VB Math (maybe problematic)

            'the negative flag is set to the most significant bit of the result
            SetFlag(FLAGS6502.N, temp And &H80US)

            'load the result into the accumulator (its 8bit)
            A = temp And &HFFUS

            'this function has potential to require an aditional clock cycle
            Return 1
        End Function

        Public Function SAX() As Byte
            ' 1. Calculate the value to store (A AND X)
            Dim result As Byte = CByte(A And X)

            ' 2. Write the result to memory
            ' (addr_abs is calculated by the addressing mode function before this is called)
            Write(addr_abs, result)

            ' 3. SAX never affects flags and always returns 0 cycles to the main loop
            Return 0
        End Function

        Public Function SBC() As Byte
            Fetch()

            'operation in 16bit domain to capture carry out

            'invert the bottom 8 bits with xor
            Dim value As UInt16 = fetched 'Xor &HFF 'Because VB hates you
            value = value Xor &HFFUS

            'temp = A + value + GetFlag(FLAGS6502.C)
            temp = MathHelpers.SafeAddition16(A, value)
            temp = MathHelpers.SafeAddition16(temp, GetFlag(FLAGS6502.C))

            SetFlag(FLAGS6502.C, temp And &HFF00US)
            SetFlag(FLAGS6502.Z, (temp And &HFFUS) = 0)
            SetFlag(FLAGS6502.V, ((temp Xor A) And (temp Xor value)) And &H80US)
            SetFlag(FLAGS6502.N, temp And &H80US)
            A = temp And &HFF

            Return 1
        End Function

        Public Function ALR() As Byte
            ' 1. Perform AND with the immediate data
            Dim data As Byte = Read(addr_abs)
            A = A And data

            ' 2. Perform LSR logic on the Accumulator
            ' The bit shifted out (bit 0) goes into the Carry flag
            SetFlag(FLAGS6502.C, (A And &H1US) <> 0)

            ' Perform the shift right (bit 7 becomes 0)
            A = CByte(A >> 1)

            ' 3. Update Zero and Negative flags based on the final result
            ' (Note: Negative flag will always be 0 because of the shift)
            SetFlag(FLAGS6502.Z, A = 0)
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)

            Return 0
        End Function


        Public Function ANC() As Byte
            ' 1. Perform standard AND with immediate data
            ' (addr_abs is set to PC by the IMM addressing mode)
            Dim data As Byte = Read(addr_abs)
            A = A And data

            ' 2. Update Zero and Negative flags based on result
            SetFlag(FLAGS6502.Z, A = 0)
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)

            ' 3. Update Carry flag to match the Negative flag (bit 7)
            SetFlag(FLAGS6502.C, GetFlag(FLAGS6502.N))

            Return 0
        End Function

        Public Function AND_() As Byte
            Fetch()
            A = A And fetched
            SetFlag(FLAGS6502.Z, A = &H0)
            SetFlag(FLAGS6502.N, A And &H80)
            Return 1
        End Function

        Public Function ARR() As Byte
            ' 1. Perform AND with the immediate data
            Dim data As Byte = Read(addr_abs)
            Dim result As Byte = CByte(A And data)

            ' 2. Perform ROR logic
            ' Bit 7 of the result is filled by the OLD Carry flag
            A = CByte((result >> 1) Or (CInt(GetFlag(FLAGS6502.C)) << 7))

            ' 3. Update Standard Flags
            SetFlag(FLAGS6502.Z, A = 0)
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)

            ' 4. Unique ARR Flag Logic (Critical for nestest 2026)
            ' Carry flag is set to bit 6 of the result
            SetFlag(FLAGS6502.C, (A And &H40US) <> 0)

            ' Overflow flag is set if (bit 6 XOR bit 5) of the result is 1
            Dim bit6 As Integer = (A >> 6) And 1
            Dim bit5 As Integer = (A >> 5) And 1
            SetFlag(FLAGS6502.V, (bit6 Xor bit5) <> 0)

            Return 0
        End Function


        Public Function ASL() As Byte
            Fetch()

            'temp = fetched << 1 'Because VB hates you
            temp = MathHelpers.SafeShiftLeft16(fetched, 1)

            SetFlag(FLAGS6502.C, (temp And &HFF00US) > &H0)
            SetFlag(FLAGS6502.Z, (temp And &HFFUS) = &H0)
            SetFlag(FLAGS6502.N, temp And &H80US)
            If MatchParentDelegate(lookup(opcode).AddrMode, AddressOf Me.IMP) Then
                A = temp And &HFFUS
            Else
                Write(addr_abs, temp And &HFFUS)
            End If
            Return 0
        End Function

        Public Function AXS() As Byte
            ' 1. Fetch immediate data
            Dim data As Byte = Read(addr_abs)

            ' 2. AXS logic: (A AND X) - data
            ' This works like CMP; it doesn't use the Carry flag as input
            Dim combined As Byte = CByte(A And X)
            Dim result As Integer = CInt(combined) - CInt(data)

            ' 3. Update Flags
            ' Carry flag is set if (A AND X) >= data (no borrow occurred)
            SetFlag(FLAGS6502.C, combined >= data)

            ' Update X and other flags
            X = CByte(result And &HFF)
            SetFlag(FLAGS6502.Z, X = 0)
            SetFlag(FLAGS6502.N, (X And &H80US) <> 0)

            Return 0
        End Function

        Public Function BCC() As Byte
            If GetFlag(FLAGS6502.C) = 0 Then
                IncrementCycles()

                addr_abs = MathHelpers.SafeAddition16(PC, addr_rel)

                If (addr_abs And &HFF00US) <> (PC And &HFF00US) Then
                    IncrementCycles()
                End If
                PC = addr_abs
            End If
            Return 0
        End Function

        Public Function BCS() As Byte
            If GetFlag(FLAGS6502.C) = 1 Then
                IncrementCycles()

                addr_abs = MathHelpers.SafeAddition16(PC, addr_rel)

                If (addr_abs And &HFF00US) <> (PC And &HFF00US) Then
                    IncrementCycles()
                End If
                PC = addr_abs
            End If
            Return 0
        End Function

        Public Function BEQ() As Byte
            If GetFlag(FLAGS6502.Z) = 1 Then
                IncrementCycles()

                addr_abs = MathHelpers.SafeAddition16(PC, addr_rel)

                If (addr_abs And &HFF00US) <> (PC And &HFF00US) Then
                    IncrementCycles()
                End If
                PC = addr_abs
            End If
            Return 0
        End Function

        Public Function BIT() As Byte
            Fetch()
            temp = A And fetched
            SetFlag(FLAGS6502.Z, (temp And &HFFUS) = 0)
            SetFlag(FLAGS6502.N, fetched And (1 << 7))
            SetFlag(FLAGS6502.V, fetched And (1 << 6))
            Return 0
        End Function

        Public Function BMI() As Byte
            If GetFlag(FLAGS6502.N) = 1 Then
                IncrementCycles()

                addr_abs = MathHelpers.SafeAddition16(PC, addr_rel)

                If (addr_abs And &HFF00US) <> (PC And &HFF00US) Then
                    IncrementCycles()
                End If
                PC = addr_abs
            End If
            Return 0
        End Function

        Public Function BNE() As Byte
            If GetFlag(FLAGS6502.Z) = 0 Then
                IncrementCycles()

                addr_abs = MathHelpers.SafeAddition16(PC, addr_rel)

                If (addr_abs And &HFF00US) <> (PC And &HFF00US) Then
                    IncrementCycles()
                End If
                PC = addr_abs
            End If
            Return 0
        End Function

        Public Function BPL() As Byte
            If GetFlag(FLAGS6502.N) = 0 Then
                IncrementCycles()

                addr_abs = MathHelpers.SafeAddition16(PC, addr_rel)

                If (addr_abs And &HFF00US) <> (PC And &HFF00US) Then
                    IncrementCycles()
                End If
                PC = addr_abs
            End If
            Return 0
        End Function

        Public Function BRK() As Byte
            IncrementProgramCounter()

            SetFlag(FLAGS6502.I, 1)
            Write(&H100US + StackPointer, (PC >> 8) And &HFFUS)
            DecrementStackpointer()
            Write(&H100US + StackPointer, PC And &HFFUS)
            DecrementStackpointer()

            SetFlag(FLAGS6502.B, 1)
            Write(&H100US + StackPointer, Status)
            DecrementStackpointer()
            SetFlag(FLAGS6502.B, 0)

            'vb math is the anus, cant do this here due to bytes 'PC = PC Or IO(&HFFFE) [might not compute the math value]
            'PC = CUShort(Read(&HFFFFUS))
            'PC <<= 8
            'PC = PC Or CUShort(Read(&HFFFEUS))

            Dim lo As Byte = Read(&HFFFEUS)
            Dim hi As Byte = Read(&HFFFFUS)
            PC = (CUShort(hi) << 8) Or CUShort(lo)

            Return 0
        End Function

        Public Function BVC() As Byte
            If GetFlag(FLAGS6502.V) = 0 Then
                IncrementCycles()

                addr_abs = MathHelpers.SafeAddition16(PC, addr_rel)

                If (addr_abs And &HFF00US) <> (PC And &HFF00US) Then
                    IncrementCycles()
                End If
                PC = addr_abs
            End If
            Return 0
        End Function

        Public Function BVS() As Byte
            If GetFlag(FLAGS6502.V) = 1 Then
                IncrementCycles()

                addr_abs = MathHelpers.SafeAddition16(PC, addr_rel)

                If (addr_abs And &HFF00US) <> (PC And &HFF00US) Then
                    IncrementCycles()
                End If
                PC = addr_abs
            End If
            Return 0
        End Function

        Public Function CLC() As Byte
            SetFlag(FLAGS6502.C, False)
            Return 0
        End Function

        Public Function CLD() As Byte
            SetFlag(FLAGS6502.D, False)
            Return 0
        End Function

        Public Function CLI() As Byte
            SetFlag(FLAGS6502.I, False)
            Return 0
        End Function

        Public Function CLV() As Byte
            SetFlag(FLAGS6502.V, False)
            Return 0
        End Function

        Public Function CMP() As Byte
            Fetch()
            temp = MathHelpers.SafeSubtract16(A, fetched) 'should be compat byte - byte
            SetFlag(FLAGS6502.C, A >= fetched)
            SetFlag(FLAGS6502.Z, (temp And &HFFUS) = &H0US)
            SetFlag(FLAGS6502.N, temp And &H80US)
            Return 1
        End Function

        Public Function CPX() As Byte
            Fetch()
            temp = MathHelpers.SafeSubtract16(X, fetched) 'Should be compat byte - byte
            SetFlag(FLAGS6502.C, X >= fetched)
            SetFlag(FLAGS6502.Z, (temp And &HFF) = &H0)
            SetFlag(FLAGS6502.N, temp And &H80)
            Return 0
        End Function

        Public Function CPY() As Byte
            Fetch()
            temp = MathHelpers.SafeSubtract16(Y, fetched) 'Should be compat byte - byte 'will this ever go negative
            SetFlag(FLAGS6502.C, Y >= fetched)
            SetFlag(FLAGS6502.Z, (temp And &HFF) = &H0)
            SetFlag(FLAGS6502.N, temp And &H80)
            Return 0
        End Function

        Public Function DCP() As Byte
            ' 1. Fetch data from memory
            Dim data As Byte = Read(addr_abs)

            ' 2. Decrement the value (with 8-bit wrap)
            data = CByte((CInt(data) - 1) And &HFF)

            ' 3. Write modified value back to memory
            Write(addr_abs, data)

            ' 4. Compare (CMP) logic: A - data
            ' Carry flag is set if A is greater than or equal to data
            SetFlag(FLAGS6502.C, A >= data)

            ' Update Z and N based on the subtraction result
            Dim temp As Byte = CByte((CInt(A) - CInt(data)) And &HFF)
            SetFlag(FLAGS6502.Z, temp = 0)
            SetFlag(FLAGS6502.N, (temp And &H80US) <> 0)

            Return 0
        End Function

        'Decrement ASM
        Public Function DEC() As Byte
            Fetch()
            temp = MathHelpers.SafeSubtract16(fetched, 1) 'will this ever go negative?

            Write(addr_abs, temp And &HFFUS)
            SetFlag(FLAGS6502.Z, (temp And &HFFUS) = &H0US)
            SetFlag(FLAGS6502.N, temp And &H80US)
            Return 0
        End Function

        Public Function DEX() As Byte
            'X -= 1 'will this ever go negative
            X = MathHelpers.SafeDecrementByte(X)
            SetFlag(FLAGS6502.Z, X = &H0)
            SetFlag(FLAGS6502.N, X And &H80)
            Return 0
        End Function

        Public Function DEY() As Byte
            'Y -= 1 'will this ever go negative
            Y = MathHelpers.SafeDecrementByte(Y)
            SetFlag(FLAGS6502.Z, Y = &H0)
            SetFlag(FLAGS6502.N, Y And &H80)
            Return 0
        End Function

        'Logical Xor
        Public Function EOR() As Byte
            Fetch()
            A = A Xor fetched
            SetFlag(FLAGS6502.Z, A = &H0)
            SetFlag(FLAGS6502.N, A And &H80)
            Return 1
        End Function

        'Increment
        Public Function INC() As Byte
            Fetch()
            temp = MathHelpers.SafeAddition16(fetched, 1)

            Write(addr_abs, temp And &HFF)
            SetFlag(FLAGS6502.Z, (temp And &HFF) = &H0)
            SetFlag(FLAGS6502.N, temp And &H80)
            Return 0
        End Function

        Public Function ISB() As Byte
            ' 1. Fetch data from memory
            Dim data As Byte = Read(addr_abs)

            ' 2. Increment the value (with 8-bit wrap)
            data = CByte((CInt(data) + 1) And &HFF)

            ' 3. Write modified value back to memory
            Write(addr_abs, data)

            ' 4. SBC logic (Subtract from Accumulator with Carry)
            ' Remember: SBC on 6502 is A - M - (1 - Carry)
            ' We use UShort to handle bit manipulation correctly for flags
            Dim value As UShort = CUShort(data Xor &HFFUS) ' Invert bits for subtraction logic

            Dim temp As UShort = CUShort(CUShort(A) + value + CUShort(GetFlag(FLAGS6502.C)))

            ' Update Overflow flag (V)
            SetFlag(FLAGS6502.V, ((temp Xor CUShort(A)) And (temp Xor value) And &H80US) <> 0)

            ' Update Carry flag (C)
            SetFlag(FLAGS6502.C, temp > 255)

            ' Update Accumulator
            A = CByte(temp And &HFFUS)

            ' Update Negative (N) and Zero (Z)
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)
            SetFlag(FLAGS6502.Z, A = 0)

            Return 0
        End Function

        'Increment X
        Public Function INX() As Byte
            X = MathHelpers.SafeIncrementByte(X)
            SetFlag(FLAGS6502.Z, X = &H0)
            SetFlag(FLAGS6502.N, X And &H80)
            Return 0
        End Function

        'Increment Y
        Public Function INY() As Byte
            Y = MathHelpers.SafeIncrementByte(Y)
            SetFlag(FLAGS6502.Z, Y = &H0)
            SetFlag(FLAGS6502.N, Y And &H80)
            Return 0
        End Function

        'Jump to location
        Public Function JMP() As Byte
            PC = addr_abs
            Return 0
        End Function

        'Jump to Sub
        Public Function JSR() As Byte
            DecrementProgramCounter()

            Write(&H100US + StackPointer, (PC >> 8) And &HFF)
            DecrementStackpointer()
            Write(&H100US + StackPointer, PC And &HFF)
            DecrementStackpointer()

            PC = addr_abs
            Return 0
        End Function

        Public Function KIL() As Byte
            ' The CPU is jammed. It will no longer fetch new opcodes.
            ' We keep the PC pointing at the KIL instruction.
            PC = CUShort(PC - 1)

            ' Optional: Log this event as it usually indicates a 
            ' crash or a jump into a non-code data segment.
            Debug.WriteLine($"CPU JAMMED at address: {PC:X4}")

            Return 0
        End Function

        Public Function LAX() As Byte
            ' 1. Fetch data from memory
            Dim data As Byte = Read(addr_abs)

            ' 2. Load into both registers
            A = data
            X = data

            ' 3. Update flags based on the loaded data
            SetFlag(FLAGS6502.Z, A = 0)
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)

            ' LAX can potentially take an extra cycle if a page boundary is crossed 
            ' in certain addressing modes (ABY, IZY), which is handled by the 
            ' return value of the addressing mode function in most architectures.
            Return 1
        End Function

        Public Function LAS() As Byte
            ' 1. Fetch data from memory
            Dim data As Byte = Read(addr_abs)

            ' 2. LAS logic: A, X, and SP all become (data AND StackPointer)
            Dim result As Byte = CByte(data And StackPointer)
            A = result
            X = result
            StackPointer = result

            ' 3. Update Status Flags
            SetFlag(FLAGS6502.Z, A = 0)
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)

            ' LAS supports the page-boundary cycle penalty
            Return 1
        End Function

        'load accumulator
        Public Function LDA() As Byte
            Fetch()
            A = fetched
            SetFlag(FLAGS6502.Z, A = &H0)
            SetFlag(FLAGS6502.N, A And &H80)
            Return 1
        End Function

        Public Function LDX() As Byte
            Fetch()
            X = fetched
            SetFlag(FLAGS6502.Z, X = &H0)
            SetFlag(FLAGS6502.N, X And &H80)
            Return 1
        End Function

        Public Function LDY() As Byte
            Fetch()
            Y = fetched
            SetFlag(FLAGS6502.Z, Y = &H0)
            SetFlag(FLAGS6502.N, Y And &H80)
            Return 1
        End Function

        Public Function LSR() As Byte
            Fetch()
            SetFlag(FLAGS6502.C, fetched And &H1US)
            temp = MathHelpers.SafeShiftRight16(fetched, 1)
            SetFlag(FLAGS6502.Z, (temp And &HFFUS) = 0US)
            SetFlag(FLAGS6502.N, temp And &H80US)
            If MatchParentDelegate(lookup(opcode).AddrMode, AddressOf Me.IMP) Then
                A = temp And &HFF
            Else
                Write(addr_abs, temp And &HFF)
            End If
            Return 0
        End Function

        Public Function NOP() As Byte
            Select Case opcode
                Case &H1C, &H3C, &H5C, &H7C, &HDC, &HFC
                    Return 1
            End Select
            Return 0
        End Function

        'bitwise or
        Public Function ORA() As Byte
            Fetch()
            A = A Or fetched
            SetFlag(FLAGS6502.Z, A = &H0)
            SetFlag(FLAGS6502.N, A And &H80)
            Return 1
        End Function

        Public Function PHA() As Byte
            Write(&H100US + StackPointer, A)
            DecrementStackpointer()
            Return 0
        End Function

        'push status
        Public Function PHP() As Byte
            Write(&H100US + StackPointer, Status Or FLAGS6502.B Or FLAGS6502.U)
            SetFlag(FLAGS6502.B, 0)
            SetFlag(FLAGS6502.U, 0)
            DecrementStackpointer()
            Return 0
        End Function

        'pop a
        Public Function PLA() As Byte
            IncrementStackpointer()
            A = Read(&H100US + StackPointer)
            SetFlag(FLAGS6502.Z, A = 0)
            SetFlag(FLAGS6502.N, A And &H80)
            Return 0
        End Function

        'pop status of the stack
        Public Function PLP() As Byte
            IncrementStackpointer()
            Status = Read(&H100US + StackPointer)
            SetFlag(FLAGS6502.U, 1)
            Return 0
        End Function

        Public Function RLA() As Byte
            ' 1. Fetch data from memory
            Dim data As Byte = Read(addr_abs)

            ' 2. ROL (Rotate Left) Logic
            ' Save the bit that will shift out (bit 7)
            Dim bit7 As Byte = If((data And &H80US) <> 0, 1, 0)

            ' Perform the shift and bring in the OLD Carry flag to bit 0
            ' (Assuming GetFlag returns 1 or 0)
            data = CByte(((data << 1) Or GetFlag(FLAGS6502.C)) And &HFFUS)

            ' 3. Update Carry flag with the bit that was shifted out
            SetFlag(FLAGS6502.C, bit7 = 1)

            ' 4. Write back the rotated value
            Write(addr_abs, data)

            ' 5. AND logic (AND with Accumulator)
            A = A And data

            ' 6. Final flag updates based on the Accumulator
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)
            SetFlag(FLAGS6502.Z, A = 0)

            Return 0
        End Function

        'rotate left
        Public Function ROL() As Byte
            Fetch()

            temp = MathHelpers.SafeShiftLeft16(fetched, 1) Or GetFlag(FLAGS6502.C)

            SetFlag(FLAGS6502.C, temp And &HFF00)
            SetFlag(FLAGS6502.Z, (temp And &HFF) = &H0)
            SetFlag(FLAGS6502.N, temp And &H80)
            If MatchParentDelegate(lookup(opcode).AddrMode, AddressOf Me.IMP) Then
                A = temp And &HFF
            Else
                Write(addr_abs, temp And &HFF)
            End If
            Return 0
        End Function

        'rotate
        Public Function ROR() As Byte
            Fetch()

            'temp = (GetFlag(FLAGS6502.C) << 7) Or (fetched >> 1) 'Because VB hates you
            temp = MathHelpers.SafeShiftLeft16(GetFlag(FLAGS6502.C), 7) Or MathHelpers.SafeShiftRight16(fetched, 1)

            SetFlag(FLAGS6502.C, fetched And &H1)
            SetFlag(FLAGS6502.Z, (temp And &HFF) = &H0)
            SetFlag(FLAGS6502.N, temp And &H80)
            If MatchParentDelegate(lookup(opcode).AddrMode, AddressOf Me.IMP) Then
                A = temp And &HFF
            Else
                Write(addr_abs, temp And &HFF)
            End If
            Return 0
        End Function

        Public Function RRA() As Byte
            ' 1. Fetch data from memory
            Dim data As Byte = Read(addr_abs)

            ' 2. ROR (Rotate Right) Logic
            ' Save the bit that will shift out (bit 0)
            Dim bit0 As Byte = If((data And &H1US) <> 0, 1, 0)

            ' Perform the shift and bring in the OLD Carry flag to bit 7
            data = CByte((data >> 1) Or (CInt(GetFlag(FLAGS6502.C)) << 7))

            ' 3. Update Carry flag for the NEXT step (the ADC portion)
            SetFlag(FLAGS6502.C, bit0 = 1)

            ' 4. Write back the rotated value
            Write(addr_abs, data)

            ' 5. ADC logic (Add with Carry)
            ' We use UShort to handle the carry out (bit 8)
            ' Note: On NES 6502, Decimal mode is ignored.
            Dim temp As UShort = CUShort(CUShort(A) + CUShort(data) + CUShort(GetFlag(FLAGS6502.C)))

            ' Update Overflow flag (V)
            ' (A ^ temp) & (data ^ temp) & 0x0080
            SetFlag(FLAGS6502.V, ((CUShort(A) Xor temp) And (CUShort(data) Xor temp) And &H80US) <> 0)

            ' Update Carry flag (C)
            SetFlag(FLAGS6502.C, temp > 255)

            ' Update Accumulator
            A = CByte(temp And &HFFUS)

            ' Update Negative (N) and Zero (Z)
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)
            SetFlag(FLAGS6502.Z, A = 0)

            Return 0
        End Function

        Public Function RTI() As Byte
            IncrementStackpointer()
            Status = Read(&H100US + StackPointer)
            Status = Status And (Not FLAGS6502.B)
            Status = Status And (Not FLAGS6502.U)

            IncrementStackpointer()
            PC = Read(&H100US + StackPointer)
            IncrementStackpointer()
            PC = PC Or MathHelpers.SafeShiftLeft16(Read(&H100US + StackPointer), 8)

            Return 0
        End Function

        Public Function RTS() As Byte
            IncrementStackpointer()
            PC = Read(&H100US + StackPointer)
            IncrementStackpointer()
            PC = PC Or MathHelpers.SafeShiftLeft16(Read(&H100US + StackPointer), 8)

            IncrementProgramCounter() 'Havent seen this incremented in this function on other emulators.
            Return 0
        End Function

        'set carry flag
        Public Function SEC() As Byte
            SetFlag(FLAGS6502.C, True)
            Return 0
        End Function

        Public Function SED() As Byte
            SetFlag(FLAGS6502.D, True)
            Return 0
        End Function

        Public Function SEI() As Byte
            SetFlag(FLAGS6502.I, True)
            Return 0
        End Function

        Public Function SHA() As Byte
            ' SHA logic: result = A AND X AND (High Byte of the target address + 1)
            ' Note: addr_abs must have been calculated by your IZY addressing mode first
            Dim highBytePlus1 As Byte = CByte(((addr_abs >> 8) And &HFFUS) + 1)
            Dim result As Byte = CByte(A And X And highBytePlus1)

            Write(addr_abs, result)
            Return 0
        End Function

        Public Function SHX() As Byte
            ' SHX logic: X AND (High Byte of target address + 1)
            Dim targetHighByte As Byte = CByte((addr_abs >> 8) And &HFFUS)
            Dim result As Byte = CByte(X And (targetHighByte + 1))

            Write(addr_abs, result)
            Return 0
        End Function

        Public Function SHY() As Byte
            ' SHY logic: Y AND (High Byte of target address + 1)
            Dim targetHighByte As Byte = CByte((addr_abs >> 8) And &HFFUS)
            Dim result As Byte = CByte(Y And (targetHighByte + 1))

            Write(addr_abs, result)
            Return 0
        End Function

        Public Function SLO() As Byte
            ' 1. Fetch the data from memory (Address calculated by addressing mode)
            Dim data As Byte = Read(addr_abs)

            ' 2. ASL (Shift Left) logic
            ' Set Carry flag based on bit 7 before shifting
            SetFlag(FLAGS6502.C, (data And &H80US) <> 0)

            ' Perform the shift
            data = CByte((data << 1) And &HFFUS)

            ' Update N and Z flags based on the shift result
            SetFlag(FLAGS6502.N, (data And &H80US) <> 0)
            SetFlag(FLAGS6502.Z, data = 0)

            ' 3. Write back the shifted value to memory
            Write(addr_abs, data)

            ' 4. ORA logic (OR with Accumulator)
            A = A Or data

            ' 5. Final flag updates based on the Accumulator
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)
            SetFlag(FLAGS6502.Z, A = 0)

            Return 0
        End Function

        Public Function SRE() As Byte
            ' 1. Fetch data from memory
            Dim data As Byte = Read(addr_abs)

            ' 2. LSR (Logical Shift Right) Logic
            ' Set Carry flag to the bit that is being shifted out (bit 0)
            SetFlag(FLAGS6502.C, (data And &H1US) <> 0)

            ' Perform the shift (bit 7 automatically becomes 0)
            data = CByte(data >> 1)

            ' 3. Write back the shifted value
            Write(addr_abs, data)

            ' 4. EOR logic (Exclusive OR with Accumulator)
            A = A Xor data

            ' 5. Final flag updates based on the Accumulator
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)
            SetFlag(FLAGS6502.Z, A = 0)

            Return 0
        End Function

        Public Function STA() As Byte
            Write(addr_abs, A)
            Return 0
        End Function

        Public Function STX() As Byte
            Write(addr_abs, X)
            Return 0
        End Function

        Public Function STY() As Byte
            Write(addr_abs, Y)
            Return 0
        End Function

        Public Function TAX() As Byte
            X = A
            SetFlag(FLAGS6502.Z, X = &H0)
            SetFlag(FLAGS6502.N, X And &H80)
            Return 0
        End Function

        Public Function TAY() As Byte
            Y = A
            SetFlag(FLAGS6502.Z, Y = &H0)
            SetFlag(FLAGS6502.N, Y And &H80)
            Return 0
        End Function

        Public Function TAS() As Byte
            ' 1. Transfer A AND X to the Stack Pointer
            ' This is the only instruction that modifies the Stack Pointer directly via math
            StackPointer = CByte(A And X)

            ' 2. Calculate the value to store in memory
            ' Logic: StackPointer AND (High Byte of target address + 1)
            Dim targetHighByte As Byte = CByte((addr_abs >> 8) And &HFFUS)
            Dim result As Byte = CByte(StackPointer And (targetHighByte + 1))

            ' 3. Write the result to the address calculated by ABY
            Write(addr_abs, result)

            Return 0
        End Function


        Public Function TSX() As Byte
            X = StackPointer
            SetFlag(FLAGS6502.Z, X = &H0)
            SetFlag(FLAGS6502.N, X And &H80)
            Return 0
        End Function

        Public Function TXA() As Byte
            A = X
            SetFlag(FLAGS6502.Z, A = &H0)
            SetFlag(FLAGS6502.N, A And &H80)
            Return 0
        End Function

        Public Function TXS() As Byte
            StackPointer = X
            Return 0
        End Function

        Public Function TYA() As Byte
            A = Y
            SetFlag(FLAGS6502.Z, A = &H0)
            SetFlag(FLAGS6502.N, A And &H80)
            Return 0
        End Function

        Public Function XAA() As Byte
            ' 1. Fetch immediate data
            Dim data As Byte = Read(addr_abs)

            ' 2. XAA Logic: (A OR magic) AND X AND data
            ' Most emulators use &HFF for the "magic" constant to maintain stability.
            ' This effectively makes the operation: A = X AND data
            Dim magic As Byte = &HFFUS
            A = CByte((A Or magic) And X And data)

            ' 3. Update Status Flags
            SetFlag(FLAGS6502.Z, A = 0)
            SetFlag(FLAGS6502.N, (A And &H80US) <> 0)

            Return 0
        End Function

        'illegal opcodes
        Public Function XXX() As Byte
            Return 0
        End Function
#End Region

#Region "Helper functions"
        Public Function complete() As Boolean
            Return cycles = 0
        End Function
#End Region

        Public Sub Reset()
            'get address to set program counter to
            addr_abs = &HFFFCUS
            Dim lo As Byte = Read(addr_abs + 0)
            Dim hi As Byte = Read(addr_abs + 1)

            'set the pc
            PC = (CUShort(hi) << 8) Or CUShort(lo)

            'reset the registers
            A = 0
            X = 0
            Y = 0
            StackPointer = &HFD
            Status = &H0 Or FLAGS6502.U

            'clear help
            addr_rel = &H0US
            addr_abs = &H0US
            fetched = &H0

            'reset time
            cycles = 8
        End Sub

        Public Sub IRQ()
            If GetFlag(FLAGS6502.I) = 0 Then
                'push the program counter to the stack
                Write(&H100 + StackPointer, (PC >> 8) And &HFFUS)
                DecrementStackpointer()
                Write(&H100 + StackPointer, PC And &HFFUS)
                DecrementStackpointer()

                'push status register to the stack
                SetFlag(FLAGS6502.B, 0)
                SetFlag(FLAGS6502.U, 1)
                SetFlag(FLAGS6502.I, 1)
                Write(&H100 + StackPointer, Status)
                DecrementStackpointer()

                'read new program counter location from fixed address
                addr_abs = &HFFFEUS
                Dim lo As Byte = Read(addr_abs + 0)
                Dim hi As Byte = Read(addr_abs + 1)
                PC = (CUShort(hi) << 8) Or CUShort(lo)

                'IRQ take time
                cycles = 7
            End If
        End Sub
        Public Sub NMI()
            'Debug.WriteLine(String.Format("CPU NMI() called! PC=0x{0:X4}, SP=0x{1:X2}", PC, StackPointer))

            Write(&H100US + StackPointer, (PC >> 8) And &HFFUS)
            DecrementStackpointer()
            Write(&H100US + StackPointer, PC And &HFFUS)
            DecrementStackpointer()

            SetFlag(FLAGS6502.B, 0)
            SetFlag(FLAGS6502.U, 1)
            SetFlag(FLAGS6502.I, 1)
            Write(&H100US + StackPointer, Status)
            DecrementStackpointer()

            addr_abs = &HFFFAUS
            Dim lo As Byte = Read(addr_abs + 0)
            Dim hi As Byte = Read(addr_abs + 1)
            PC = (CUShort(hi) << 8) Or CUShort(lo)

            cycles = 8
        End Sub
        Public Sub Clock()
            If cycles = 0 Then
                opcode = Read(PC)

                'IF LOGMODE
                'blah
                'ENDIF

                'always set the unused status flag bit to 1
                SetFlag(FLAGS6502.U, True)

                'increment the program counter
                IncrementProgramCounter()

                'get the starting number of cycles
                cycles = lookup(opcode).cycles

                'perform fetch of intermediate data using the required addressing mode
                Dim additional_cycle1 As Byte = lookup(opcode).AddrMode()
                'perform operation
                Dim additional_cycle2 As Byte = lookup(opcode).Operate()

                '// The addressmode And opcode may have altered the number
                '// of cycles this instruction requires before its completed
                cycles += (additional_cycle1 And additional_cycle2)

                'always set the unused status flag bit to 1
                SetFlag(FLAGS6502.U, True)

                'IF LOGMODE
                'blah
                'ENDIF
            End If

            'Increment the global clock count
            clock_count += 1

            'Decrement the number of cycles
            cycles -= 1
            Return
        End Sub



        Public fetched As Byte = &H0
        Public temp As UInt16 = &H0US
        Public temp2 As UInt16 = &H0US 'ROR
        Public addr_abs As UInt16 = &H0US
        Public addr_rel As UInt16 = &H0US
        Public opcode As Byte = &H0
        Public cycles As Byte = &H0
        Public clock_count As UInt32 = 0UI

        Public ReadOnly Property Debug_PC As UInt16
            Get
                Return PC  ' Change "pc" to whatever you named your program counter
            End Get
        End Property

        Public ReadOnly Property Debug_SP As Byte
            Get
                Return StackPointer  ' Change "stkp" to whatever you named your stack pointer
            End Get
        End Property

        Public ReadOnly Property Debug_A As Byte
            Get
                Return A  ' Change "a" to whatever you named your accumulator
            End Get
        End Property

        Public ReadOnly Property Debug_X As Byte
            Get
                Return X  ' Change "x" to whatever you named your X register
            End Get
        End Property

        Public ReadOnly Property Debug_Y As Byte
            Get
                Return Y  ' Change "y" to whatever you named your Y register
            End Get
        End Property

        Public ReadOnly Property Debug_Status As Byte
            Get
                Return Status  ' Change "status" to whatever you named your status register
            End Get
        End Property
        Public ReadOnly Property Debug_ClockCount As UInteger
            Get
                Return clock_count
            End Get
        End Property

        Public Function GetFlag(ByVal f As FLAGS6502) As Byte
            If (Status And f) > 0 Then
                Return 1
            End If
            Return 0
        End Function
        Public Sub SetFlag(ByVal f As FLAGS6502, ByVal v As Boolean)
            If v Then
                Status = Status Or f
            Else
                Status = Status And (Not f)
            End If
        End Sub

#Region "READ / WRITE"
        Public Function Read(ByVal addr As UInt16) As Byte
            Return Bus.cpuRead(addr, False)
        End Function
        Public Sub Write(ByVal addr As UInt16, ByVal data As Byte)
            Bus.cpuWrite(addr, data)
        End Sub
#End Region

    End Class

End Namespace