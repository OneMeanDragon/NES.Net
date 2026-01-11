Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' High-performance 16-bit bitfield using only UInt16 operations
    ''' </summary>
    Public Structure BitField16
        Private _value As UInt16

        Public Sub New(initialValue As UInt16)
            _value = initialValue
        End Sub

        Public Property Value As UInt16
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _value
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(val As UInt16)
                _value = val
            End Set
        End Property

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function GetBit(bitIndex As Integer) As Boolean
            If bitIndex < 0 OrElse bitIndex > 15 Then Return False
            Return (_value And (1US << bitIndex)) <> 0
        End Function

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetBit(bitIndex As Integer)
            If bitIndex >= 0 AndAlso bitIndex <= 15 Then
                _value = _value Or CUShort(1 << bitIndex)
            End If
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub ClearBit(bitIndex As Integer)
            If bitIndex >= 0 AndAlso bitIndex <= 15 Then
                _value = _value And CUShort(Not (1 << bitIndex))
            End If
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub WriteBit(bitIndex As Integer, value As Boolean)
            If bitIndex >= 0 AndAlso bitIndex <= 15 Then
                If value Then
                    SetBit(bitIndex)
                Else
                    ClearBit(bitIndex)
                End If
            End If
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub Clear()
            _value = 0
        End Sub

        ' Implicit conversions
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Widening Operator CType(value As UInt16) As BitField16
            Return New BitField16(value)
        End Operator

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Widening Operator CType(field As BitField16) As UInt16
            Return field._value
        End Operator

        Public Overrides Function ToString() As String
            Return Convert.ToString(_value, 2).PadLeft(16, "0"c)
        End Function
    End Structure

End Namespace