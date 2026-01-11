Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' High-performance 8-bit bitfield using only byte operations (no boolean array overhead)
    ''' </summary>
    Public Structure BitField8
        Private _value As Byte

        ''' <summary>
        ''' Create a bitfield with initial value
        ''' </summary>
        Public Sub New(Optional initialValue As Byte = &H0)
            _value = initialValue
        End Sub

        ''' <summary>
        ''' Get/Set the entire byte value
        ''' </summary>
        Public Property Value As Byte
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _value
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(val As Byte)
                _value = val
            End Set
        End Property

        ''' <summary>
        ''' Get a specific bit (0-7)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function GetBit(bitIndex As Integer) As Boolean
            If bitIndex < 0 OrElse bitIndex > 7 Then Return False
            Return (_value And (1 << bitIndex)) <> 0
        End Function

        ''' <summary>
        ''' Set a specific bit to 1
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetBit(bitIndex As Integer)
            If bitIndex >= 0 AndAlso bitIndex <= 7 Then
                _value = _value Or CByte(1 << bitIndex)
            End If
        End Sub

        ''' <summary>
        ''' Clear a specific bit to 0
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub ClearBit(bitIndex As Integer)
            If bitIndex >= 0 AndAlso bitIndex <= 7 Then
                _value = _value And CByte(Not (1 << bitIndex))
            End If
        End Sub

        ''' <summary>
        ''' Toggle a specific bit
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub ToggleBit(bitIndex As Integer)
            If bitIndex >= 0 AndAlso bitIndex <= 7 Then
                _value = _value Xor CByte(1 << bitIndex)
            End If
        End Sub

        ''' <summary>
        ''' Set a bit to a specific value (True/False)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub WriteBit(bitIndex As Integer, value As Boolean)
            If bitIndex >= 0 AndAlso bitIndex <= 7 Then
                If value Then
                    SetBit(bitIndex)
                Else
                    ClearBit(bitIndex)
                End If
            End If
        End Sub

        ''' <summary>
        ''' Clear all bits to 0
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub Clear()
            _value = 0
        End Sub

        ''' <summary>
        ''' Set all bits to 1
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetAll()
            _value = &HFF
        End Sub

        ''' <summary>
        ''' Count how many bits are set
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function CountSetBits() As Integer
            ' Brian Kernighan's algorithm
            Dim count = 0
            Dim temp = _value
            While temp <> 0
                temp = temp And (temp - 1)
                count += 1
            End While
            Return count
        End Function

        ' Implicit conversions for convenience
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Widening Operator CType(value As Byte) As BitField8
            Return New BitField8(value)
        End Operator

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Widening Operator CType(field As BitField8) As Byte
            Return field._value
        End Operator

        ' Comparison operators
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Operator =(left As BitField8, right As BitField8) As Boolean
            Return left._value = right._value
        End Operator

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Operator <>(left As BitField8, right As BitField8) As Boolean
            Return left._value <> right._value
        End Operator

        Public Overrides Function ToString() As String
            Return Convert.ToString(_value, 2).PadLeft(8, "0"c)
        End Function

        Public Overrides Function Equals(obj As Object) As Boolean
            If TypeOf obj Is BitField8 Then
                Return _value = DirectCast(obj, BitField8)._value
            End If
            Return False
        End Function

        Public Overrides Function GetHashCode() As Integer
            Return _value.GetHashCode()
        End Function

    End Structure

End Namespace