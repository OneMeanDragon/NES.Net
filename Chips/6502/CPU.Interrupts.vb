Namespace NintendoEntertainmentSystem

    ' MOS Technology 6502 CPU - Interrupts and Clock (Partial Class)
    Partial Public NotInheritable Class CPU6502

#Region "Reset and Interrupts"
        ''' <summary>
        ''' Reset the CPU to initial state
        ''' Reads the reset vector from $FFFC-$FFFD
        ''' </summary>
        Public Sub Reset()
            ' Read reset vector
            _addrAbs = &HFFFCUS
            Dim lo = Read(_addrAbs)
            Dim hi = Read(_addrAbs + 1)
            PC = (CUShort(hi) << 8) Or lo

            ' Reset registers
            A = 0
            X = 0
            Y = 0
            SP = &HFD
            Status = StatusFlags.U

            ' Clear internal state
            _addrRel = 0
            _addrAbs = 0
            _fetched = 0

            ' Reset takes 8 cycles
            _cycles = 8
        End Sub

        ''' <summary>
        ''' Interrupt Request (IRQ)
        ''' Maskable hardware interrupt
        ''' Vector: $FFFE-$FFFF
        ''' </summary>
        Public Sub IRQ()
            ' IRQ can be disabled by the I flag
            If GetFlag(StatusFlags.I) = 0 Then
                ' Push program counter to stack
                PushWord(PC)

                ' Push status register to stack
                SetFlag(StatusFlags.B, False)
                SetFlag(StatusFlags.U, True)
                SetFlag(StatusFlags.I, True)
                Push(Status)

                ' Read IRQ vector
                _addrAbs = &HFFFEUS
                Dim lo = Read(_addrAbs)
                Dim hi = Read(_addrAbs + 1)
                PC = (CUShort(hi) << 8) Or lo

                ' IRQ takes 7 cycles
                _cycles = 7
            End If
        End Sub

        ''' <summary>
        ''' Non-Maskable Interrupt (NMI)
        ''' Cannot be disabled, triggered by PPU VBlank
        ''' Vector: $FFFA-$FFFB
        ''' </summary>
        Public Sub NMI()
            ' Push program counter to stack
            PushWord(PC)

            ' Push status register to stack
            SetFlag(StatusFlags.B, False)
            SetFlag(StatusFlags.U, True)
            SetFlag(StatusFlags.I, True)
            Push(Status)

            ' Read NMI vector
            _addrAbs = &HFFFAUS
            Dim lo = Read(_addrAbs)
            Dim hi = Read(_addrAbs + 1US)
            PC = (CUShort(hi) << 8) Or lo

            ' NMI takes 8 cycles
            _cycles = 8
        End Sub
#End Region

#Region "Clock"
        ''' <summary>
        ''' Execute one CPU clock cycle
        ''' Fetches and executes instructions when cycles reach 0
        ''' </summary>
        Public Sub Clock()
            ' Only fetch new instruction when previous one is complete
            If _cycles = 0 Then
                ' Fetch opcode from current PC location
                _opcode = Read(PC)

                ' Always set unused flag
                SetFlag(StatusFlags.U, True)

                ' Increment program counter
                PC += 1US
                'Debug.WriteLine($"ProgramCounter: {PC}")
                'If PC >= &HFFFFUS Then
                '    Debug.WriteLine($"ProgramCounter MAX: {PC}")
                'End If

                ' Get base cycle count for this instruction
                _cycles = _instructions(_opcode).Cycles

                ' Execute addressing mode (may add extra cycle)
                Dim additionalCycle1 = _instructions(_opcode).AddressingMode()

                ' Execute instruction operation (may add extra cycle)
                Dim additionalCycle2 = _instructions(_opcode).Operate()

                ' Add any additional cycles (only if both return 1)
                _cycles += (additionalCycle1 And additionalCycle2)

                ' Always set unused flag
                SetFlag(StatusFlags.U, True)
            End If

            ' Increment global clock count
            ClockCount += 1

            ' Decrement cycles remaining for current instruction
            _cycles -= 1
        End Sub
#End Region

    End Class

End Namespace