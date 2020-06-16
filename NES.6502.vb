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
            InitializeHandler(0, "BRK", AddressOf BRK, AddressOf IMM, 7)
            InitializeHandler(1, "ORA", AddressOf ORA, AddressOf IZX, 6)
            InitializeHandler(2, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(3, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(4, "???", AddressOf NOP, AddressOf IMP, 3)
            InitializeHandler(5, "ORA", AddressOf ORA, AddressOf ZP0, 3)
            InitializeHandler(6, "ASL", AddressOf ASL, AddressOf ZP0, 5)
            InitializeHandler(7, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(8, "PHP", AddressOf PHP, AddressOf IMP, 3)
            InitializeHandler(9, "ORA", AddressOf ORA, AddressOf IMM, 2)
            InitializeHandler(10, "ASL", AddressOf ASL, AddressOf IMP, 2)
            InitializeHandler(11, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(12, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(13, "ORA", AddressOf ORA, AddressOf ABS, 4)
            InitializeHandler(14, "ASL", AddressOf ASL, AddressOf ABS, 6)
            InitializeHandler(15, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(16, "BPL", AddressOf BPL, AddressOf REL, 2)
            InitializeHandler(17, "ORA", AddressOf ORA, AddressOf IZY, 5)
            InitializeHandler(18, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(19, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(20, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(21, "ORA", AddressOf ORA, AddressOf ZPX, 4)
            InitializeHandler(22, "ASL", AddressOf ASL, AddressOf ZPX, 6)
            InitializeHandler(23, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(24, "CLC", AddressOf CLC, AddressOf IMP, 2)
            InitializeHandler(25, "ORA", AddressOf ORA, AddressOf ABY, 4)
            InitializeHandler(26, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(27, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(28, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(29, "ORA", AddressOf ORA, AddressOf ABX, 4)
            InitializeHandler(30, "ASL", AddressOf ASL, AddressOf ABX, 7)
            InitializeHandler(31, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(32, "JSR", AddressOf JSR, AddressOf ABS, 6)
            InitializeHandler(33, "AND", AddressOf AND_, AddressOf IZX, 6)
            InitializeHandler(34, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(35, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(36, "BIT", AddressOf BIT, AddressOf ZP0, 3)
            InitializeHandler(37, "AND", AddressOf AND_, AddressOf ZP0, 3)
            InitializeHandler(38, "ROL", AddressOf ROL, AddressOf ZP0, 5)
            InitializeHandler(39, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(40, "PLP", AddressOf PLP, AddressOf IMP, 4)
            InitializeHandler(41, "AND", AddressOf AND_, AddressOf IMM, 2)
            InitializeHandler(42, "ROL", AddressOf ROL, AddressOf IMP, 2)
            InitializeHandler(43, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(44, "BIT", AddressOf BIT, AddressOf ABS, 4)
            InitializeHandler(45, "AND", AddressOf AND_, AddressOf ABS, 4)
            InitializeHandler(46, "ROL", AddressOf ROL, AddressOf ABS, 6)
            InitializeHandler(47, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(48, "BMI", AddressOf BMI, AddressOf REL, 2)
            InitializeHandler(49, "AND", AddressOf AND_, AddressOf IZY, 5)
            InitializeHandler(50, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(51, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(52, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(53, "AND", AddressOf AND_, AddressOf ZPX, 4)
            InitializeHandler(54, "ROL", AddressOf ROL, AddressOf ZPX, 6)
            InitializeHandler(55, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(56, "SEC", AddressOf SEC, AddressOf IMP, 2)
            InitializeHandler(57, "AND", AddressOf AND_, AddressOf ABY, 4)
            InitializeHandler(58, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(59, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(60, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(61, "AND", AddressOf AND_, AddressOf ABX, 4)
            InitializeHandler(62, "ROL", AddressOf ROL, AddressOf ABX, 7)
            InitializeHandler(63, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(64, "RTI", AddressOf RTI, AddressOf IMP, 6)
            InitializeHandler(65, "EOR", AddressOf EOR, AddressOf IZX, 6)
            InitializeHandler(66, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(67, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(68, "???", AddressOf NOP, AddressOf IMP, 3)
            InitializeHandler(69, "EOR", AddressOf EOR, AddressOf ZP0, 3)
            InitializeHandler(70, "LSR", AddressOf LSR, AddressOf ZP0, 5)
            InitializeHandler(71, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(72, "PHA", AddressOf PHA, AddressOf IMP, 3)
            InitializeHandler(73, "EOR", AddressOf EOR, AddressOf IMM, 2)
            InitializeHandler(74, "LSR", AddressOf LSR, AddressOf IMP, 2)
            InitializeHandler(75, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(76, "JMP", AddressOf JMP, AddressOf ABS, 3)
            InitializeHandler(77, "EOR", AddressOf EOR, AddressOf ABS, 4)
            InitializeHandler(78, "LSR", AddressOf LSR, AddressOf ABS, 6)
            InitializeHandler(79, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(80, "BVC", AddressOf BVC, AddressOf REL, 2)
            InitializeHandler(81, "EOR", AddressOf EOR, AddressOf IZY, 5)
            InitializeHandler(82, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(83, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(84, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(85, "EOR", AddressOf EOR, AddressOf ZPX, 4)
            InitializeHandler(86, "LSR", AddressOf LSR, AddressOf ZPX, 6)
            InitializeHandler(87, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(88, "CLI", AddressOf CLI, AddressOf IMP, 2)
            InitializeHandler(89, "EOR", AddressOf EOR, AddressOf ABY, 4)
            InitializeHandler(90, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(91, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(92, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(93, "EOR", AddressOf EOR, AddressOf ABX, 4)
            InitializeHandler(94, "LSR", AddressOf LSR, AddressOf ABX, 7)
            InitializeHandler(95, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(96, "RTS", AddressOf RTS, AddressOf IMP, 6)
            InitializeHandler(97, "ADC", AddressOf ADC, AddressOf IZX, 6)
            InitializeHandler(98, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(99, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(100, "???", AddressOf NOP, AddressOf IMP, 3)
            InitializeHandler(101, "ADC", AddressOf ADC, AddressOf ZP0, 3)
            InitializeHandler(102, "ROR", AddressOf ROR, AddressOf ZP0, 5)
            InitializeHandler(103, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(104, "PLA", AddressOf PLA, AddressOf IMP, 4)
            InitializeHandler(105, "ADC", AddressOf ADC, AddressOf IMM, 2)
            InitializeHandler(106, "ROR", AddressOf ROR, AddressOf IMP, 2)
            InitializeHandler(107, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(108, "JMP", AddressOf JMP, AddressOf IND, 5)
            InitializeHandler(109, "ADC", AddressOf ADC, AddressOf ABS, 4)
            InitializeHandler(110, "ROR", AddressOf ROR, AddressOf ABS, 6)
            InitializeHandler(111, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(112, "BVS", AddressOf BVS, AddressOf REL, 2)
            InitializeHandler(113, "ADC", AddressOf ADC, AddressOf IZY, 5)
            InitializeHandler(114, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(115, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(116, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(117, "ADC", AddressOf ADC, AddressOf ZPX, 4)
            InitializeHandler(118, "ROR", AddressOf ROR, AddressOf ZPX, 6)
            InitializeHandler(119, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(120, "SEI", AddressOf SEI, AddressOf IMP, 2)
            InitializeHandler(121, "ADC", AddressOf ADC, AddressOf ABY, 4)
            InitializeHandler(122, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(123, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(124, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(125, "ADC", AddressOf ADC, AddressOf ABX, 4)
            InitializeHandler(126, "ROR", AddressOf ROR, AddressOf ABX, 7)
            InitializeHandler(127, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(128, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(129, "STA", AddressOf STA, AddressOf IZX, 6)
            InitializeHandler(130, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(131, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(132, "STY", AddressOf STY, AddressOf ZP0, 3)
            InitializeHandler(133, "STA", AddressOf STA, AddressOf ZP0, 3)
            InitializeHandler(134, "STX", AddressOf STX, AddressOf ZP0, 3)
            InitializeHandler(135, "???", AddressOf XXX, AddressOf IMP, 3)
            InitializeHandler(136, "DEY", AddressOf DEY, AddressOf IMP, 2)
            InitializeHandler(137, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(138, "TXA", AddressOf TXA, AddressOf IMP, 2)
            InitializeHandler(139, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(140, "STY", AddressOf STY, AddressOf ABS, 4)
            InitializeHandler(141, "STA", AddressOf STA, AddressOf ABS, 4)
            InitializeHandler(142, "STX", AddressOf STX, AddressOf ABS, 4)
            InitializeHandler(143, "???", AddressOf XXX, AddressOf IMP, 4)
            InitializeHandler(144, "BCC", AddressOf BCC, AddressOf REL, 2)
            InitializeHandler(145, "STA", AddressOf STA, AddressOf IZY, 6)
            InitializeHandler(146, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(147, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(148, "STY", AddressOf STY, AddressOf ZPX, 4)
            InitializeHandler(149, "STA", AddressOf STA, AddressOf ZPX, 4)
            InitializeHandler(150, "STX", AddressOf STX, AddressOf ZPY, 4)
            InitializeHandler(151, "???", AddressOf XXX, AddressOf IMP, 4)
            InitializeHandler(152, "TYA", AddressOf TYA, AddressOf IMP, 2)
            InitializeHandler(153, "STA", AddressOf STA, AddressOf ABY, 5)
            InitializeHandler(154, "TXS", AddressOf TXS, AddressOf IMP, 2)
            InitializeHandler(155, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(156, "???", AddressOf NOP, AddressOf IMP, 5)
            InitializeHandler(157, "STA", AddressOf STA, AddressOf ABX, 5)
            InitializeHandler(158, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(159, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(160, "LDY", AddressOf LDY, AddressOf IMM, 2)
            InitializeHandler(161, "LDA", AddressOf LDA, AddressOf IZX, 6)
            InitializeHandler(162, "LDX", AddressOf LDX, AddressOf IMM, 2)
            InitializeHandler(163, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(164, "LDY", AddressOf LDY, AddressOf ZP0, 3)
            InitializeHandler(165, "LDA", AddressOf LDA, AddressOf ZP0, 3)
            InitializeHandler(166, "LDX", AddressOf LDX, AddressOf ZP0, 3)
            InitializeHandler(167, "???", AddressOf XXX, AddressOf IMP, 3)
            InitializeHandler(168, "TAY", AddressOf TAY, AddressOf IMP, 2)
            InitializeHandler(169, "LDA", AddressOf LDA, AddressOf IMM, 2)
            InitializeHandler(170, "TAX", AddressOf TAX, AddressOf IMP, 2)
            InitializeHandler(171, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(172, "LDY", AddressOf LDY, AddressOf ABS, 4)
            InitializeHandler(173, "LDA", AddressOf LDA, AddressOf ABS, 4)
            InitializeHandler(174, "LDX", AddressOf LDX, AddressOf ABS, 4)
            InitializeHandler(175, "???", AddressOf XXX, AddressOf IMP, 4)
            InitializeHandler(176, "BCS", AddressOf BCS, AddressOf REL, 2)
            InitializeHandler(177, "LDA", AddressOf LDA, AddressOf IZY, 5)
            InitializeHandler(178, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(179, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(180, "LDY", AddressOf LDY, AddressOf ZPX, 4)
            InitializeHandler(181, "LDA", AddressOf LDA, AddressOf ZPX, 4)
            InitializeHandler(182, "LDX", AddressOf LDX, AddressOf ZPY, 4)
            InitializeHandler(183, "???", AddressOf XXX, AddressOf IMP, 4)
            InitializeHandler(184, "CLV", AddressOf CLV, AddressOf IMP, 2)
            InitializeHandler(185, "LDA", AddressOf LDA, AddressOf ABY, 4)
            InitializeHandler(186, "TSX", AddressOf TSX, AddressOf IMP, 2)
            InitializeHandler(187, "???", AddressOf XXX, AddressOf IMP, 4)
            InitializeHandler(188, "LDY", AddressOf LDY, AddressOf ABX, 4)
            InitializeHandler(189, "LDA", AddressOf LDA, AddressOf ABX, 4)
            InitializeHandler(190, "LDX", AddressOf LDX, AddressOf ABY, 4)
            InitializeHandler(191, "???", AddressOf XXX, AddressOf IMP, 4)
            InitializeHandler(192, "CPY", AddressOf CPY, AddressOf IMM, 2)
            InitializeHandler(193, "CMP", AddressOf CMP, AddressOf IZX, 6)
            InitializeHandler(194, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(195, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(196, "CPY", AddressOf CPY, AddressOf ZP0, 3)
            InitializeHandler(197, "CMP", AddressOf CMP, AddressOf ZP0, 3)
            InitializeHandler(198, "DEC", AddressOf DEC, AddressOf ZP0, 5)
            InitializeHandler(199, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(200, "INY", AddressOf INY, AddressOf IMP, 2)
            InitializeHandler(201, "CMP", AddressOf CMP, AddressOf IMM, 2)
            InitializeHandler(202, "DEX", AddressOf DEX, AddressOf IMP, 2)
            InitializeHandler(203, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(204, "CPY", AddressOf CPY, AddressOf ABS, 4)
            InitializeHandler(205, "CMP", AddressOf CMP, AddressOf ABS, 4)
            InitializeHandler(206, "DEC", AddressOf DEC, AddressOf ABS, 6)
            InitializeHandler(207, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(208, "BNE", AddressOf BNE, AddressOf REL, 2)
            InitializeHandler(209, "CMP", AddressOf CMP, AddressOf IZY, 5)
            InitializeHandler(210, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(211, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(212, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(213, "CMP", AddressOf CMP, AddressOf ZPX, 4)
            InitializeHandler(214, "DEC", AddressOf DEC, AddressOf ZPX, 6)
            InitializeHandler(215, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(216, "CLD", AddressOf CLD, AddressOf IMP, 2)
            InitializeHandler(217, "CMP", AddressOf CMP, AddressOf ABY, 4)
            InitializeHandler(218, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(219, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(220, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(221, "CMP", AddressOf CMP, AddressOf ABX, 4)
            InitializeHandler(222, "DEC", AddressOf DEC, AddressOf ABX, 7)
            InitializeHandler(223, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(224, "CPX", AddressOf CPX, AddressOf IMM, 2)
            InitializeHandler(225, "SBC", AddressOf SBC, AddressOf IZX, 6)
            InitializeHandler(226, "???", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(227, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(228, "CPX", AddressOf CPX, AddressOf ZP0, 3)
            InitializeHandler(229, "SBC", AddressOf SBC, AddressOf ZP0, 3)
            InitializeHandler(230, "INC", AddressOf INC, AddressOf ZP0, 5)
            InitializeHandler(231, "???", AddressOf XXX, AddressOf IMP, 5)
            InitializeHandler(232, "INX", AddressOf INX, AddressOf IMP, 2)
            InitializeHandler(233, "SBC", AddressOf SBC, AddressOf IMM, 2)
            InitializeHandler(234, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(235, "???", AddressOf SBC, AddressOf IMP, 2)
            InitializeHandler(236, "CPX", AddressOf CPX, AddressOf ABS, 4)
            InitializeHandler(237, "SBC", AddressOf SBC, AddressOf ABS, 4)
            InitializeHandler(238, "INC", AddressOf INC, AddressOf ABS, 6)
            InitializeHandler(239, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(240, "BEQ", AddressOf BEQ, AddressOf REL, 2)
            InitializeHandler(241, "SBC", AddressOf SBC, AddressOf IZY, 5)
            InitializeHandler(242, "???", AddressOf XXX, AddressOf IMP, 2)
            InitializeHandler(243, "???", AddressOf XXX, AddressOf IMP, 8)
            InitializeHandler(244, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(245, "SBC", AddressOf SBC, AddressOf ZPX, 4)
            InitializeHandler(246, "INC", AddressOf INC, AddressOf ZPX, 6)
            InitializeHandler(247, "???", AddressOf XXX, AddressOf IMP, 6)
            InitializeHandler(248, "SED", AddressOf SED, AddressOf IMP, 2)
            InitializeHandler(249, "SBC", AddressOf SBC, AddressOf ABY, 4)
            InitializeHandler(250, "NOP", AddressOf NOP, AddressOf IMP, 2)
            InitializeHandler(251, "???", AddressOf XXX, AddressOf IMP, 7)
            InitializeHandler(252, "???", AddressOf NOP, AddressOf IMP, 4)
            InitializeHandler(253, "SBC", AddressOf SBC, AddressOf ABX, 4)
            InitializeHandler(254, "INC", AddressOf INC, AddressOf ABX, 7)
            InitializeHandler(255, "???", AddressOf XXX, AddressOf IMP, 7)
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub
#End Region

#Region "RAM BUS"
        Private Shared Property Bus() As clsBus
        Public Sub ConnectBus(ByRef RamBus As clsBus)
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
            Dim lo As UInt16 = Read(PC)
            IncrementProgramCounter()
            Dim hi As UInt16 = Read(PC)
            IncrementProgramCounter()
            addr_abs = (hi << 8) Or lo
            Return &H0
        End Function

        Public Function ABX() As Byte
            Dim lo As UInt16 = Read(PC)
            IncrementProgramCounter()
            Dim hi As UInt16 = Read(PC)
            IncrementProgramCounter()

            addr_abs = MathHelpers.SafeAddition16((hi << 8) Or lo, X)

            If (addr_abs And &HFF00US) <> (hi << 8) Then
                Return 1
            Else
                Return 0
            End If
        End Function

        Public Function ABY() As Byte
            Dim lo As UInt16 = Read(PC)
            IncrementProgramCounter()
            Dim hi As UInt16 = Read(PC)
            IncrementProgramCounter()

            addr_abs = MathHelpers.SafeAddition16((hi << 8) Or lo, Y)

            If (addr_abs And &HFF00US) <> (hi << 8) Then
                Return 1
            Else
                Return 0
            End If
        End Function

        Public Function IND() As Byte
            Dim ptr_lo As UInt16 = Read(PC)
            IncrementProgramCounter()
            Dim ptr_hi As UInt16 = Read(PC)
            IncrementProgramCounter()

            Dim ptr As UInt16 = (ptr_hi << 8) Or ptr_lo

            If ptr_lo = &HFFUS Then
                addr_abs = MathHelpers.SafeOr16(MathHelpers.SafeShiftLeft16(Read(ptr And &HFF00US), 8), Read(ptr + 0))
            Else
                addr_abs = MathHelpers.SafeOr16(MathHelpers.SafeShiftLeft16(Read(MathHelpers.SafeAddition16(ptr, 1)), 8), Read(ptr + 0))
            End If

            Return 0
        End Function

        Public Function IZX() As Byte
            Dim t As UInt16 = Read(PC)
            IncrementProgramCounter()

            Dim lo As UInt16 = Read((MathHelpers.SafeAddition16(t, X)) And &HFFUS)
            Dim hi As UInt16 = Read((MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(t, X), 1)) And &HFFUS)

            addr_abs = (hi << 8) Or lo

            Return 0
        End Function

        Public Function IZY() As Byte
            Dim t As UInt16 = Read(PC)
            IncrementProgramCounter()

            Dim lo As UInt16 = Read(t And &HFFUS)
            Dim hi As UInt16 = Read((MathHelpers.SafeAddition16(t, 1)) And &HFFUS)

            addr_abs = MathHelpers.SafeAddition16(MathHelpers.SafeShiftLeft16(hi, 8) Or lo, Y)

            If (addr_abs And &HFF00US) <> (hi << 8) Then
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

        Public Function AND_() As Byte
            Fetch()
            A = A And fetched
            SetFlag(FLAGS6502.Z, A = &H0)
            SetFlag(FLAGS6502.N, A And &H80)
            Return 1
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
            PC = Read(&HFFFFUS) : PC = PC << 8 : PC = PC Or Read(&HFFFEUS)
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
            Dim lo As UInt16 = Read(addr_abs + 0)
            Dim hi As UInt16 = Read(addr_abs + 1)

            'set the pc
            PC = (hi << 8) Or lo

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
                Dim lo As UInt16 = Read(addr_abs + 0)
                Dim hi As UInt16 = Read(addr_abs + 1)
                PC = (hi << 8) Or lo

                'IRQ take time
                cycles = 7
            End If
        End Sub
        Public Sub NMI()
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
            Dim lo As UInt16 = Read(addr_abs + 0)
            Dim hi As UInt16 = Read(addr_abs + 1)
            PC = (hi << 8) Or lo

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