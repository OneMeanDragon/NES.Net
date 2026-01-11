Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    Public Structure LoopyRegister
        ' The underlying 16-bit storage
        Public Bits As BitField16

        ''' <summary> Bits 0-4: Coarse X Scroll (0-31 tiles) </summary>
        Public Property CoarseX As Byte
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                ' Mask: 0000 0000 0001 1111 (&H1F)
                Return CByte(Bits.Value And &H1FUS)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Byte)
                ' Clear bits 0-4 and set them
                Bits.Value = (Bits.Value And &HFFE0US) Or (value And &H1FUS)
            End Set
        End Property

        ''' <summary> Bits 5-9: Coarse Y Scroll (0-31 tiles) </summary>
        Public Property CoarseY As Byte
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                ' Mask: 0000 0011 1110 0000 (&H3E0)
                Return CByte((Bits.Value And &H3E0US) >> 5)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Byte)
                ' Clear bits 5-9 and set them
                Bits.Value = (Bits.Value And &HFC1FUS) Or (CUShort(value And &H1FUS) << 5)
            End Set
        End Property

        ''' <summary> Bit 10: Nametable X Select </summary>
        Public Property NametableX As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(10)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(10, value)
            End Set
        End Property

        ''' <summary> Bit 11: Nametable Y Select </summary>
        Public Property NametableY As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(11)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(11, value)
            End Set
        End Property

        ''' <summary> Bits 12-14: Fine Y Scroll (0-7 pixels) </summary>
        Public Property FineY As Byte
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                ' Mask: 0111 0000 0000 0000 (&H7000)
                Return CByte((Bits.Value And &H7000US) >> 12)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Byte)
                ' Clear bits 12-14 and set them
                Bits.Value = (Bits.Value And &H8FFFUS) Or (CUShort(value And &H7US) << 12)
            End Set
        End Property

        ''' <summary> Bit 15: Unused </summary>
        Public Property Unused As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(15)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(15, value)
            End Set
        End Property

        ''' <summary> The raw 16-bit register value (reg) </summary>
        Public Property Reg As UShort
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.Value
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As UShort)
                Bits.Value = value
            End Set
        End Property
    End Structure

End Namespace