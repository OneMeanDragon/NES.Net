Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    Public Structure PpuStatusRegister
        ' This is the "reg" part of the union
        Public Bits As BitField8

        ''' <summary>
        ''' Bits 0-4: Unused (Open Bus behavior)
        ''' </summary>
        Public Property Unused As Byte
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                ' Return the lower 5 bits (mask: 0001 1111)
                Return CByte(Bits.Value And &H1F)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Byte)
                ' Clear lower 5 bits and set them to new value
                Bits.Value = CByte((Bits.Value And &HE0) Or (value And &H1F))
            End Set
        End Property

        ''' <summary>
        ''' Bit 5: Sprite Overflow
        ''' </summary>
        Public Property SpriteOverflow As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(5)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(5, value)
            End Set
        End Property

        ''' <summary>
        ''' Bit 6: Sprite Zero Hit
        ''' </summary>
        Public Property SpriteZeroHit As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(6)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(6, value)
            End Set
        End Property

        ''' <summary>
        ''' Bit 7: Vertical Blank
        ''' </summary>
        Public Property VerticalBlank As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(7)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(7, value)
            End Set
        End Property

        ' Helper to treat the whole thing as a byte (mimics the "reg" part of the union)
        Public Property Reg As Byte
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.Value
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Byte)
                Bits.Value = value
            End Set
        End Property
    End Structure

End Namespace