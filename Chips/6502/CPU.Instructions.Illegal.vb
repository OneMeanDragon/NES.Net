Namespace NintendoEntertainmentSystem

    ' MOS Technology 6502 CPU - Illegal/Undocumented Instructions (Partial Class)
    ' These are unofficial opcodes that combine multiple operations
    Partial Public NotInheritable Class CPU6502

#Region "Illegal Instructions - Combination Operations"
        ''' <summary>KIL - Halt the CPU (Illegal)</summary>
        Friend Function KIL() As Byte
            PC -= 1US
            Debug.WriteLine($"CPU JAMMED at address: {PC:X4}")
            Return 0
        End Function

        ''' <summary>LAX - Load A and X (Illegal)</summary>
        Friend Function LAX() As Byte
            Fetch()
            A = _fetched
            X = _fetched
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            Return 1
        End Function

        ''' <summary>SAX - Store A AND X (Illegal)</summary>
        Friend Function SAX() As Byte
            Write(_addrAbs, A And X)
            Return 0
        End Function

        ''' <summary>DCP - Decrement then Compare (Illegal)</summary>
        Friend Function DCP() As Byte
            Dim data = Read(_addrAbs)
            data = (data - 1UI) And &HFFUI
            Write(_addrAbs, data)
            SetFlag(StatusFlags.C, A >= data)
            Dim temp = (A - data) And &HFFUI
            SetFlag(StatusFlags.Z, temp = 0)
            SetFlag(StatusFlags.N, (temp And &H80UI) <> 0)
            Return 0
        End Function

        ''' <summary>ISB - Increment then Subtract with Carry (Illegal)</summary>
        Friend Function ISB() As Byte
            Dim data = Read(_addrAbs)
            data = (data + 1UI) And &HFFUI
            Write(_addrAbs, data)

            ' SBC logic
            Dim value As Byte = data Xor &HFFUI
            _temp = CUShort(A) + value + GetFlag(StatusFlags.C)
            SetFlag(StatusFlags.V, ((_temp Xor A) And (_temp Xor value) And &H80UI) <> 0)
            SetFlag(StatusFlags.C, _temp > &HFFUS)
            A = _temp And &HFFUS
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            SetFlag(StatusFlags.Z, A = 0)
            Return 0
        End Function

        ''' <summary>SLO - Shift Left then OR (Illegal)</summary>
        Friend Function SLO() As Byte
            Dim data = Read(_addrAbs)
            SetFlag(StatusFlags.C, (data And &H80UI) <> 0)
            data = (CUShort(data) << 1) And &HFFUI
            Write(_addrAbs, data)
            A = A Or data
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            SetFlag(StatusFlags.Z, A = 0)
            Return 0
        End Function

        ''' <summary>RLA - Rotate Left then AND (Illegal)</summary>
        Friend Function RLA() As Byte
            Dim data = Read(_addrAbs)
            Dim bit7 = If((data And &H80UI) <> 0, 1, 0)
            data = ((CUShort(data) << 1) Or GetFlag(StatusFlags.C)) And &HFFUI
            SetFlag(StatusFlags.C, bit7 = 1)
            Write(_addrAbs, data)
            A = A And data
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            SetFlag(StatusFlags.Z, A = 0)
            Return 0
        End Function

        ''' <summary>SRE - Shift Right then EOR (Illegal)</summary>
        Friend Function SRE() As Byte
            Dim data = Read(_addrAbs)
            SetFlag(StatusFlags.C, (data And 1UI) <> 0)
            data = data >> 1UI
            Write(_addrAbs, data)
            A = A Xor data
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            SetFlag(StatusFlags.Z, A = 0)
            Return 0
        End Function

        ''' <summary>RRA - Rotate Right then Add with Carry (Illegal)</summary>
        Friend Function RRA() As Byte
            Dim data = Read(_addrAbs)
            Dim bit0 = If((data And 1UI) <> 0, 1, 0)
            data = (data >> 1UI) Or (GetFlag(StatusFlags.C) << 7UI)
            SetFlag(StatusFlags.C, bit0 = 1)
            Write(_addrAbs, data)

            ' ADC logic
            _temp = CUShort(A) + data + GetFlag(StatusFlags.C)
            SetFlag(StatusFlags.V, ((CUShort(A) Xor _temp) And (CUShort(data) Xor _temp) And &H80UI) <> 0)
            SetFlag(StatusFlags.C, _temp > &HFFUS)
            A = _temp And &HFFUS
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            SetFlag(StatusFlags.Z, A = 0)
            Return 0
        End Function
#End Region

#Region "Illegal Instructions - Immediate Operations"
        ''' <summary>ANC - AND then copy N to C (Illegal)</summary>
        Friend Function ANC() As Byte
            Dim data = Read(_addrAbs)
            A = A And data
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            SetFlag(StatusFlags.C, GetFlag(StatusFlags.N))
            Return 0
        End Function

        ''' <summary>ALR - AND then Logical Shift Right (Illegal)</summary>
        Friend Function ALR() As Byte
            Dim data = Read(_addrAbs)
            A = A And data
            SetFlag(StatusFlags.C, (A And 1UI) <> 0)
            A = A >> 1UI
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            Return 0
        End Function

        ''' <summary>ARR - AND then Rotate Right (Illegal)</summary>
        Friend Function ARR() As Byte
            Dim data = Read(_addrAbs)
            Dim result = A And data
            A = (result >> 1UI) Or (GetFlag(StatusFlags.C) << 7UI)
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            SetFlag(StatusFlags.C, (A And &H40UI) <> 0)

            ' Overflow flag special logic
            Dim bit6 = (A >> 6UI) And 1
            Dim bit5 = (A >> 5UI) And 1
            SetFlag(StatusFlags.V, (bit6 Xor bit5) <> 0)
            Return 0
        End Function

        ''' <summary>AXS - AND X with A, then subtract (Illegal)</summary>
        Friend Function AXS() As Byte
            Dim data = Read(_addrAbs)
            Dim combined = A And X
            Dim result = combined - data
            SetFlag(StatusFlags.C, combined >= data)
            X = result And &HFFUI
            SetFlag(StatusFlags.Z, X = 0)
            SetFlag(StatusFlags.N, (X And &H80UI) <> 0)
            Return 0
        End Function

        ''' <summary>XAA - Transfer X to A then AND (Illegal, unstable)</summary>
        Friend Function XAA() As Byte
            Dim data = Read(_addrAbs)
            Dim magic As Byte = &HFFUI
            A = (A Or magic) And X And data
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            Return 0
        End Function
#End Region

#Region "Illegal Instructions - Special Operations"
        ''' <summary>LAS - AND memory with stack pointer (Illegal)</summary>
        Friend Function LAS() As Byte
            Dim data = Read(_addrAbs)
            Dim result = data And SP
            A = result
            X = result
            SP = result
            SetFlag(StatusFlags.Z, A = 0)
            SetFlag(StatusFlags.N, (A And &H80UI) <> 0)
            Return 1
        End Function

        ''' <summary>SHA - Store A AND X AND (high byte + 1) (Illegal)</summary>
        Friend Function SHA() As Byte
            Dim highBytePlus1 As Byte = ((_addrAbs >> 8US) And &HFFUS) + 1UI
            Dim result As Byte = A And X And highBytePlus1
            Write(_addrAbs, result)
            Return 0
        End Function

        ''' <summary>SHX - Store X AND (high byte + 1) (Illegal)</summary>
        Friend Function SHX() As Byte
            Dim targetHighByte As Byte = (_addrAbs >> 8US) And &HFFUS
            Dim result As Byte = X And (targetHighByte + 1UI)
            Write(_addrAbs, result)
            Return 0
        End Function

        ''' <summary>SHY - Store Y AND (high byte + 1) (Illegal)</summary>
        Friend Function SHY() As Byte
            Dim targetHighByte As Byte = (_addrAbs >> 8US) And &HFFUS
            Dim result As Byte = Y And (targetHighByte + 1UI)
            Write(_addrAbs, result)
            Return 0
        End Function

        ''' <summary>TAS - Transfer A AND X to SP, then store SP AND (high byte + 1) (Illegal)</summary>
        Friend Function TAS() As Byte
            SP = A And X
            Dim targetHighByte As Byte = (_addrAbs >> 8US) And &HFFUS
            Dim result As Byte = SP And (targetHighByte + 1UI)
            Write(_addrAbs, result)
            Return 0
        End Function
#End Region

    End Class

End Namespace