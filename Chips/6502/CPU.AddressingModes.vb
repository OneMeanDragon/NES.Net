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
            PC += 1
            Return 0
        End Function

        ''' <summary>Zero Page - Address in first 256 bytes</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ZP0() As Byte
            _addrAbs = Read(PC)
            PC += 1
            _addrAbs = _addrAbs And &HFF
            Return 0
        End Function

        ''' <summary>Zero Page, X - Zero page address + X</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ZPX() As Byte
            _addrAbs = (Read(PC) + X) And &HFF
            PC += 1
            Return 0
        End Function

        ''' <summary>Zero Page, Y - Zero page address + Y</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ZPY() As Byte
            _addrAbs = (Read(PC) + Y) And &HFF
            PC += 1
            Return 0
        End Function

        ''' <summary>Relative - Signed offset for branches</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function REL() As Byte
            _addrRel = Read(PC)
            PC += 1
            If (_addrRel And &H80) <> 0 Then
                _addrRel = _addrRel Or &HFF00US
            End If
            Return 0
        End Function

        ''' <summary>Absolute - Full 16-bit address</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ABS() As Byte
            Dim lo = Read(PC)
            PC += 1
            Dim hi = Read(PC)
            PC += 1
            _addrAbs = (CUShort(hi) << 8) Or lo
            Return 0
        End Function

        ''' <summary>Absolute, X - Absolute address + X (may cross page)</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ABX() As Byte
            Dim lo = Read(PC)
            PC += 1
            Dim hi = Read(PC)
            PC += 1
            _addrAbs = (CUShort(hi) << 8) Or lo
            _addrAbs += X
            ' Return 1 if page boundary crossed
            Return If((_addrAbs And &HFF00) <> (hi << 8), 1, 0)
        End Function

        ''' <summary>Absolute, Y - Absolute address + Y (may cross page)</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function ABY() As Byte
            Dim lo = Read(PC)
            PC += 1
            Dim hi = Read(PC)
            PC += 1
            _addrAbs = (CUShort(hi) << 8) Or lo
            _addrAbs += Y
            ' Return 1 if page boundary crossed
            Return If((_addrAbs And &HFF00) <> (hi << 8), 1, 0)
        End Function

        ''' <summary>Indirect - JMP only, reads address from memory</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function IND() As Byte
            Dim ptrLo = Read(PC)
            PC += 1
            Dim ptrHi = Read(PC)
            PC += 1
            Dim ptr = (CUShort(ptrHi) << 8) Or ptrLo

            ' Simulate 6502 page boundary bug
            If ptrLo = &HFF Then
                _addrAbs = (CUShort(Read(ptr And &HFF00)) << 8) Or Read(ptr)
            Else
                _addrAbs = (CUShort(Read(ptr + 1)) << 8) Or Read(ptr)
            End If
            Return 0
        End Function

        ''' <summary>Indexed Indirect - (Zero page + X), then read address</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function IZX() As Byte
            Dim t = Read(PC)
            PC += 1
            Dim lo = Read((t + X) And &HFF)
            Dim hi = Read((t + X + 1) And &HFF)
            _addrAbs = (CUShort(hi) << 8) Or lo
            Return 0
        End Function

        ''' <summary>Indirect Indexed - Read address from zero page, then add Y</summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Function IZY() As Byte
            Dim t = Read(PC)
            PC += 1
            Dim lo = Read(t And &HFF)
            Dim hi = Read((t + 1) And &HFF)
            _addrAbs = (CUShort(hi) << 8) Or lo
            _addrAbs += Y
            ' Return 1 if page boundary crossed
            Return If((_addrAbs And &HFF00) <> (hi << 8), 1, 0)
        End Function
#End Region

    End Class

End Namespace