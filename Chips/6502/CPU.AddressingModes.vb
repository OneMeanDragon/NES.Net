Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ' MOS Technology 6502 CPU - Addressing Modes (Partial Class)
    Partial Public NotInheritable Class CPU6502

#Region "Addressing Modes"
        ''' <summary>Implied - Operates on accumulator</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function IMP() As Byte
            _fetched = A
            Return 0
        End Function

        ''' <summary>Immediate - Value is next byte</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function IMM() As Byte
            _addrAbs = PC
            PC += 1US
            Return 0
        End Function

        ''' <summary>Zero Page - Address in first 256 bytes</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ZP0() As Byte
            _addrAbs = Read(PC)
            PC += 1US
            _addrAbs = _addrAbs And &HFFUS
            Return 0
        End Function

        ''' <summary>Zero Page, X - Zero page address + X</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ZPX() As Byte
            _addrAbs = (CUShort(Read(PC)) + X) And &HFFUS
            PC += 1US
            Return 0
        End Function

        ''' <summary>Zero Page, Y - Zero page address + Y</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ZPY() As Byte
            _addrAbs = (CUShort(Read(PC)) + Y) And &HFFUS
            PC += 1US
            Return 0
        End Function

        ''' <summary>Relative - Signed offset for branches</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function REL() As Byte
            '_addrRel = Read(PC)
            'PC += 1US
            'If (_addrRel And &H80) <> 0 Then
            '    _addrRel = _addrRel Or &HFF00US
            'End If
            'Return 0
            _addrRel = Read(PC)
            PC += 1US
            ' Sign extend: if bit 7 is set, the value is negative.
            ' If _addrRel is an Integer/Short, this makes it a proper negative number.
            If (_addrRel And &H80US) <> 0 Then
                _addrRel = _addrRel Or &HFF00US
            End If
            Return 0
        End Function

        ''' <summary>Absolute - Full 16-bit address</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ABS() As Byte
            Dim lo = Read(PC)
            PC += 1US
            Dim hi = Read(PC)
            PC += 1US
            _addrAbs = (CUShort(hi) << 8US) Or lo
            Return 0
        End Function

        ''' <summary>Absolute, X - Absolute address + X (may cross page)</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ABX() As Byte
            ABS()
            Dim baseAddr As UShort = _addrAbs
            _addrAbs = ((_addrAbs + X) And &HFFFFUS)
            If (_addrAbs And &HFF00US) <> (baseAddr And &HFF00US) Then
                Return 1
            End If
            Return 0
        End Function

        ''' <summary>Absolute, Y - Absolute address + Y (may cross page)</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ABY() As Byte
            ABS()
            Dim baseAddr As UShort = _addrAbs
            _addrAbs = ((_addrAbs + Y) And &HFFFFUS)
            If (_addrAbs And &HFF00US) <> (baseAddr And &HFF00US) Then
                Return 1
            End If
            Return 0
        End Function

        ''' <summary>Indirect - JMP only, reads address from memory</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function IND() As Byte
            ABS() ' Fetch the 16-bit pointer address
            Dim lo As Byte = Read(_addrAbs)
            ' SIMULATE HARDWARE BUG: Page wrap if pointer is at $XXFF
            Dim hiAddr As UShort
            If (_addrAbs And &HFFUS) = &HFFUS Then
                hiAddr = _addrAbs And &HFF00US ' Stay on same page
            Else
                hiAddr = _addrAbs + 1US
            End If

            Dim hi As Byte = Read(hiAddr)
            _addrAbs = (CUShort(hi) << 8US) Or lo
            Return 0
        End Function

        ''' <summary>Indexed Indirect - (Zero page + X), then read address</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function IZX() As Byte
            Dim t = Read(PC)
            PC += 1US
            Dim lo = Read((CUShort(t) + X) And &HFFUS)
            Dim hi = Read((CUShort(t) + X + 1US) And &HFFUS)
            _addrAbs = (CUShort(hi) << 8US) Or lo
            Return 0
        End Function

        ''' <summary>Indirect Indexed - Read address from zero page, then add Y</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function IZY() As Byte
            Dim t As Byte = Read(PC)
            PC += 1US
            Dim lo = Read(CUShort(t) And &HFFUS)
            Dim hi = Read((CUShort(t) + 1US) And &HFFUS)

            ' Store the base address to check for page crossing
            Dim baseAddr As UShort = (CUShort(hi) << 8) Or lo
            _addrAbs = CUShort((baseAddr + Y) And &HFFFFUS)

            ' Return 1 if page boundary crossed (high bytes differ)
            ' Using UShort cast ensures 16-bit safe comparison in .NET 10
            If (_addrAbs And &HFF00US) <> (baseAddr And &HFF00US) Then
                Return 1
            End If
            Return 0
        End Function
#End Region

    End Class

End Namespace