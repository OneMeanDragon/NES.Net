Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    Public Structure PpuControlRegister
        ' Underlying storage (1 byte)
        Public Bits As BitField8

        ''' <summary> Bit 0: Nametable X (0: $2000, 1: $2400) </summary>
        Public Property NametableX As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(0)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(0, value)
            End Set
        End Property

        ''' <summary> Bit 1: Nametable Y (0: $2000, 1: $2800) </summary>
        Public Property NametableY As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(1)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(1, value)
            End Set
        End Property

        ''' <summary> Bit 2: VRAM Address Increment (0: add 1, 1: add 32) </summary>
        Public Property IncrementMode As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(2)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(2, value)
            End Set
        End Property

        ''' <summary> Bit 3: Sprite Pattern Table Address (0: $0000, 1: $1000) </summary>
        Public Property PatternSprite As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(3)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(3, value)
            End Set
        End Property

        ''' <summary> Bit 4: Background Pattern Table Address (0: $0000, 1: $1000) </summary>
        Public Property PatternBackground As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(4)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(4, value)
            End Set
        End Property

        ''' <summary> Bit 5: Sprite Size (0: 8x8, 1: 8x16) </summary>
        Public Property SpriteSize As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(5)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(5, value)
            End Set
        End Property

        ''' <summary> Bit 6: PPU Master/Slave Select (Unused in most consoles) </summary>
        Public Property SlaveMode As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(6)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(6, value)
            End Set
        End Property

        ''' <summary> Bit 7: Enable VBlank NMI Interrupt </summary>
        Public Property EnableNmi As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(7)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(7, value)
            End Set
        End Property

        ''' <summary> The raw register value (reg) </summary>
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