
Module MathHelpers
    Private M32Helper As UInt32 = 0
    Private M64Helper As UInt64 = 0

    'Public Function BitSwap(ByVal value As UInt64, ByVal bitcount As UInt64, ByVal ander As UInt64) As UInt64
    '    Dim TempData As UInt64 = value And ander
    '    Dim OutData As UInt64 = 0
    '    If TempData = 0 Then Return 0
    '    If bitcount = 0 Then Return 0
    '    If ander <= 0 Then Return 0 '1 will rep bit-0

    '    'swap the bits
    '    For i As Integer = 0 To bitcount - 1
    '        If TempData And (1 << i) Then
    '            OutData += (1 << ((bitcount - 1) - i))
    '        End If
    '    Next
    'End Function
    Public Function SafeDecrement16(ByVal bytIn As UInt16) As UInt16
        If bytIn = 0 Then
            Return &HFFFFUI
        Else
            Return (bytIn - 1)
        End If
    End Function
    Public Function SafeDecrementByte(ByVal bytIn As Byte) As Byte
        If bytIn = 0 Then
            Return &HFFUI
        Else
            Return (bytIn - 1)
        End If
    End Function
    Public Function SafeIncrementByte(ByVal bytIn As Byte) As Byte
        If bytIn = &HFFUI Then
            Return 0
        Else
            Return (bytIn + 1)
        End If
    End Function
    Public Function SafeIncrement16(ByVal v1 As UInt16) As UInt16
        If v1 = &HFFFFUI Then
            Return 0
        Else
            Return (v1 + 1)
        End If
    End Function
    Public Function SafeSubtract16(ByVal v1 As UInt16, ByVal v2 As UInt16) As UInt16
        If v1 < v2 Then
            Return (&HFFFFUI - ((v2 - 1) - v1))
        Else
            Return (v1 - v2)
        End If
    End Function
    Public Function SafeSubtract32(ByVal v1 As UInt64, ByVal v2 As UInt64) As UInt32
        If v1 < v2 Then
            Return (&HFFFFFFFFUI - ((v2 - 1) - v1))
        Else
            Return (v1 - v2)
        End If
    End Function
    Public Function SafeAddition16(ByVal v1 As UInt32, ByVal v2 As UInt32) As UInt16
        M32Helper = v1 + v2
        Return (M32Helper And &HFFFFUS)
    End Function
    Public Function SafeAddition32(ByVal v1 As UInt64, ByVal v2 As UInt64) As UInt32
        M64Helper = v1 + v2
        Return (M64Helper And &HFFFFFFFFUI)
    End Function
    Public Function SafeShiftLeft16(ByVal v1 As UInt32, ByVal v2 As UInt32) As UInt16
        M32Helper = v1 << v2
        Return (M32Helper And &HFFFFUS)
    End Function
    Public Function SafeShiftRight16(ByVal v1 As UInt32, ByVal v2 As UInt32) As UInt16
        M32Helper = v1 >> v2
        Return (M32Helper And &HFFFFUS)
    End Function

    Public Function SafeAddition8(ByVal v1 As UInt32, ByVal v2 As UInt32) As Byte
        M32Helper = v1 + v2
        Return (M32Helper And &HFFUS)
    End Function

    Public Function SafeShiftLeft8(ByVal v1 As UInt16, ByVal v2 As UInt16) As Byte
        M32Helper = v1 << v2
        Return (M32Helper And &HFFUS)
    End Function
    Public Function SafeShiftRight8(ByVal v1 As UInt16, ByVal v2 As UInt16) As Byte
        M32Helper = v1 >> v2
        Return (M32Helper And &HFFUS)
    End Function

    Public Function SafeOr16(ByVal v1 As UInt32, ByVal v2 As UInt32) As UInt16
        M32Helper = v1 Or v2
        Return (M32Helper And &HFFFFUS)
    End Function
    Public Function SafeMul16(ByVal v1 As UInt32, ByVal v2 As UInt32) As UInt16
        M32Helper = v1 * v2
        Return (M32Helper And &HFFFFUS)
    End Function
    Public Function SafeMul32(ByVal v1 As UInt64, ByVal v2 As UInt64) As UInt32
        M64Helper = v1 * v2
        Return (M64Helper And &HFFFFFFFFUI)
    End Function

End Module
