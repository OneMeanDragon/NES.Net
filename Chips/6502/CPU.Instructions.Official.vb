Namespace NintendoEntertainmentSystem

    ' MOS Technology 6502 CPU - Official Instructions (Partial Class)
    Partial Public NotInheritable Class CPU6502

#Region "Arithmetic Instructions"
        ''' <summary>Add with Carry</summary>
        Friend Function ADC() As Byte
            Fetch()
            _temp = CUShort(A) + _fetched + GetFlag(StatusFlags.C)
            SetFlag(StatusFlags.C, _temp > 255)
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.V, ((Not (A Xor _fetched) And (A Xor _temp)) And &H80) <> 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            A = _temp And &HFF
            Return 1
        End Function

        ''' <summary>Subtract with Carry</summary>
        Friend Function SBC() As Byte
            Fetch()
            Dim value = _fetched Xor &HFF
            _temp = CUShort(A) + value + GetFlag(StatusFlags.C)
            SetFlag(StatusFlags.C, (_temp And &HFF00) <> 0)
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.V, ((_temp Xor A) And (_temp Xor value) And &H80) <> 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            A = _temp And &HFF
            Return 1
        End Function
#End Region

#Region "Logical Instructions"
        ''' <summary>Logical AND</summary>
        Friend Function AND_() As Byte
            Fetch()
            A = A And _fetched
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80) <> 0)
            Return 1
        End Function

        ''' <summary>Logical OR</summary>
        Friend Function ORA() As Byte
            Fetch()
            A = A Or _fetched
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80) <> 0)
            Return 1
        End Function

        ''' <summary>Exclusive OR</summary>
        Friend Function EOR() As Byte
            Fetch()
            A = A Xor _fetched
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80) <> 0)
            Return 1
        End Function

        ''' <summary>Bit Test</summary>
        Friend Function BIT() As Byte
            Fetch()
            _temp = A And _fetched
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.N, (_fetched And &H80) <> 0)
            SetFlag(StatusFlags.V, (_fetched And &H40) <> 0)
            Return 0
        End Function
#End Region

#Region "Shift and Rotate Instructions"
        ''' <summary>Arithmetic Shift Left</summary>
        Friend Function ASL() As Byte
            Fetch()
            _temp = CUShort(_fetched) << 1
            SetFlag(StatusFlags.C, (_temp And &HFF00) <> 0)
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            If _instructions(_opcode).ModeType = AddrMode.IMP Then
                A = _temp And &HFF
            Else
                Write(_addrAbs, _temp And &HFF)
            End If
            Return 0
        End Function

        ''' <summary>Logical Shift Right</summary>
        Friend Function LSR() As Byte
            Fetch()
            SetFlag(StatusFlags.C, (_fetched And 1) <> 0)
            _temp = _fetched >> 1
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            If _instructions(_opcode).ModeType = AddrMode.IMP Then
                A = _temp And &HFF
            Else
                Write(_addrAbs, _temp And &HFF)
            End If
            Return 0
        End Function

        ''' <summary>Rotate Left</summary>
        Friend Function ROL() As Byte
            Fetch()
            _temp = CUShort(_fetched << 1) Or GetFlag(StatusFlags.C)
            SetFlag(StatusFlags.C, (_temp And &HFF00) <> 0)
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            If _instructions(_opcode).ModeType = AddrMode.IMP Then
                A = _temp And &HFF
            Else
                Write(_addrAbs, _temp And &HFF)
            End If
            Return 0
        End Function

        ''' <summary>Rotate Right</summary>
        Friend Function ROR() As Byte
            Fetch()
            _temp = (GetFlag(StatusFlags.C) << 7) Or (_fetched >> 1)
            SetFlag(StatusFlags.C, (_fetched And 1) <> 0)
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            If _instructions(_opcode).ModeType = AddrMode.IMP Then
                A = _temp And &HFF
            Else
                Write(_addrAbs, _temp And &HFF)
            End If
            Return 0
        End Function
#End Region

#Region "Increment and Decrement Instructions"
        ''' <summary>Increment Memory</summary>
        Friend Function INC() As Byte
            Fetch()
            _temp = (_fetched + 1) And &HFF
            Write(_addrAbs, _temp)
            SetFlag(StatusFlags.Z, _temp = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Decrement Memory</summary>
        Friend Function DEC() As Byte
            Fetch()
            _temp = (_fetched - 1) And &HFF
            Write(_addrAbs, _temp)
            SetFlag(StatusFlags.Z, _temp = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Increment X Register</summary>
        Friend Function INX() As Byte
            X = (X + 1) And &HFF
            SetFlag(StatusFlags.Z, X = 0)
            SetFlag(StatusFlags.N, (X And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Decrement X Register</summary>
        Friend Function DEX() As Byte
            X = (X - 1) And &HFF
            SetFlag(StatusFlags.Z, X = 0)
            SetFlag(StatusFlags.N, (X And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Increment Y Register</summary>
        Friend Function INY() As Byte
            Y = (Y + 1) And &HFF
            SetFlag(StatusFlags.Z, Y = 0)
            SetFlag(StatusFlags.N, (Y And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Decrement Y Register</summary>
        Friend Function DEY() As Byte
            Y = (Y - 1) And &HFF
            SetFlag(StatusFlags.Z, Y = 0)
            SetFlag(StatusFlags.N, (Y And &H80) <> 0)
            Return 0
        End Function
#End Region

#Region "Compare Instructions"
        ''' <summary>Compare Accumulator</summary>
        Friend Function CMP() As Byte
            Fetch()
            _temp = A - _fetched
            SetFlag(StatusFlags.C, A >= _fetched)
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            Return 1
        End Function

        ''' <summary>Compare X Register</summary>
        Friend Function CPX() As Byte
            Fetch()
            _temp = X - _fetched
            SetFlag(StatusFlags.C, X >= _fetched)
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Compare Y Register</summary>
        Friend Function CPY() As Byte
            Fetch()
            _temp = Y - _fetched
            SetFlag(StatusFlags.C, Y >= _fetched)
            SetFlag(StatusFlags.Z, (_temp And &HFF) = 0)
            SetFlag(StatusFlags.N, (_temp And &H80) <> 0)
            Return 0
        End Function
#End Region

#Region "Load and Store Instructions"
        ''' <summary>Load Accumulator</summary>
        Friend Function LDA() As Byte
            Fetch()
            A = _fetched
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80) <> 0)
            Return 1
        End Function

        ''' <summary>Load X Register</summary>
        Friend Function LDX() As Byte
            Fetch()
            X = _fetched
            SetFlag(StatusFlags.Z, X = 0)
            SetFlag(StatusFlags.N, (X And &H80) <> 0)
            Return 1
        End Function

        ''' <summary>Load Y Register</summary>
        Friend Function LDY() As Byte
            Fetch()
            Y = _fetched
            SetFlag(StatusFlags.Z, Y = 0)
            SetFlag(StatusFlags.N, (Y And &H80) <> 0)
            Return 1
        End Function

        ''' <summary>Store Accumulator</summary>
        Friend Function STA() As Byte
            Write(_addrAbs, A)
            Return 0
        End Function

        ''' <summary>Store X Register</summary>
        Friend Function STX() As Byte
            Write(_addrAbs, X)
            Return 0
        End Function

        ''' <summary>Store Y Register</summary>
        Friend Function STY() As Byte
            Write(_addrAbs, Y)
            Return 0
        End Function
#End Region

#Region "Transfer Instructions"
        ''' <summary>Transfer A to X</summary>
        Friend Function TAX() As Byte
            X = A
            SetFlag(StatusFlags.Z, X = 0)
            SetFlag(StatusFlags.N, (X And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Transfer A to Y</summary>
        Friend Function TAY() As Byte
            Y = A
            SetFlag(StatusFlags.Z, Y = 0)
            SetFlag(StatusFlags.N, (Y And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Transfer X to A</summary>
        Friend Function TXA() As Byte
            A = X
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Transfer Y to A</summary>
        Friend Function TYA() As Byte
            A = Y
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Transfer Stack Pointer to X</summary>
        Friend Function TSX() As Byte
            X = SP
            SetFlag(StatusFlags.Z, X = 0)
            SetFlag(StatusFlags.N, (X And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Transfer X to Stack Pointer</summary>
        Friend Function TXS() As Byte
            SP = X
            Return 0
        End Function
#End Region

#Region "Stack Instructions"
        ''' <summary>Push Accumulator</summary>
        Friend Function PHA() As Byte
            Push(A)
            Return 0
        End Function

        ''' <summary>Pull Accumulator</summary>
        Friend Function PLA() As Byte
            A = Pop()
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80) <> 0)
            Return 0
        End Function

        ''' <summary>Push Processor Status</summary>
        Friend Function PHP() As Byte
            Push(Status Or StatusFlags.B Or StatusFlags.U)
            SetFlag(StatusFlags.B, False)
            SetFlag(StatusFlags.U, False)
            Return 0
        End Function

        ''' <summary>Pull Processor Status</summary>
        Friend Function PLP() As Byte
            Status = Pop()
            SetFlag(StatusFlags.U, True)
            Return 0
        End Function
#End Region

#Region "Branch Instructions"
        ''' <summary>Branch if Carry Clear</summary>
        Friend Function BCC() As Byte
            Return Branch(GetFlag(StatusFlags.C) = 0)
        End Function

        ''' <summary>Branch if Carry Set</summary>
        Friend Function BCS() As Byte
            Return Branch(GetFlag(StatusFlags.C) = 1)
        End Function

        ''' <summary>Branch if Equal (Zero Set)</summary>
        Friend Function BEQ() As Byte
            Return Branch(GetFlag(StatusFlags.Z) = 1)
        End Function

        ''' <summary>Branch if Minus (Negative Set)</summary>
        Friend Function BMI() As Byte
            Return Branch(GetFlag(StatusFlags.N) = 1)
        End Function

        ''' <summary>Branch if Not Equal (Zero Clear)</summary>
        Friend Function BNE() As Byte
            Return Branch(GetFlag(StatusFlags.Z) = 0)
        End Function

        ''' <summary>Branch if Plus (Negative Clear)</summary>
        Friend Function BPL() As Byte
            Return Branch(GetFlag(StatusFlags.N) = 0)
        End Function

        ''' <summary>Branch if Overflow Clear</summary>
        Friend Function BVC() As Byte
            Return Branch(GetFlag(StatusFlags.V) = 0)
        End Function

        ''' <summary>Branch if Overflow Set</summary>
        Friend Function BVS() As Byte
            Return Branch(GetFlag(StatusFlags.V) = 1)
        End Function

        ''' <summary>Branch helper function</summary>
        Private Function Branch(condition As Boolean) As Byte
            If condition Then
                _cycles += 1
                _addrAbs = PC + _addrRel
                If (_addrAbs And &HFF00) <> (PC And &HFF00) Then
                    _cycles += 1
                End If
                PC = _addrAbs
            End If
            Return 0
        End Function
#End Region

#Region "Jump and Call Instructions"
        ''' <summary>Jump</summary>
        Friend Function JMP() As Byte
            PC = _addrAbs
            Return 0
        End Function

        ''' <summary>Jump to Subroutine</summary>
        Friend Function JSR() As Byte
            PC -= 1
            PushWord(PC)
            PC = _addrAbs
            Return 0
        End Function

        ''' <summary>Return from Subroutine</summary>
        Friend Function RTS() As Byte
            PC = PopWord()
            PC += 1
            Return 0
        End Function

        ''' <summary>Return from Interrupt</summary>
        Friend Function RTI() As Byte
            Status = Pop()
            Status = Status And (Not StatusFlags.B)
            Status = Status And (Not StatusFlags.U)
            PC = PopWord()
            Return 0
        End Function

        ''' <summary>Break</summary>
        Friend Function BRK() As Byte
            PC += 1
            SetFlag(StatusFlags.I, True)
            PushWord(PC)
            SetFlag(StatusFlags.B, True)
            Push(Status)
            SetFlag(StatusFlags.B, False)
            PC = (CUShort(Read(&HFFFF)) << 8) Or Read(&HFFFE)
            Return 0
        End Function
#End Region

#Region "Flag Instructions"
        ''' <summary>Clear Carry</summary>
        Friend Function CLC() As Byte
            SetFlag(StatusFlags.C, False)
            Return 0
        End Function

        ''' <summary>Clear Decimal (unused on NES)</summary>
        Friend Function CLD() As Byte
            SetFlag(StatusFlags.D, False)
            Return 0
        End Function

        ''' <summary>Clear Interrupt Disable</summary>
        Friend Function CLI() As Byte
            SetFlag(StatusFlags.I, False)
            Return 0
        End Function

        ''' <summary>Clear Overflow</summary>
        Friend Function CLV() As Byte
            SetFlag(StatusFlags.V, False)
            Return 0
        End Function

        ''' <summary>Set Carry</summary>
        Friend Function SEC() As Byte
            SetFlag(StatusFlags.C, True)
            Return 0
        End Function

        ''' <summary>Set Decimal (unused on NES)</summary>
        Friend Function SED() As Byte
            SetFlag(StatusFlags.D, True)
            Return 0
        End Function

        ''' <summary>Set Interrupt Disable</summary>
        Friend Function SEI() As Byte
            SetFlag(StatusFlags.I, True)
            Return 0
        End Function

        ''' <summary>No Operation</summary>
        Friend Function NOP() As Byte
            ' Some NOPs take an extra cycle on page boundary
            Select Case _opcode
                Case &H1C, &H3C, &H5C, &H7C, &HDC, &HFC
                    Return 1
            End Select
            Return 0
        End Function
#End Region

    End Class

End Namespace