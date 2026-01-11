Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    Public Structure PpuMaskRegister
        ' The underlying storage (1 byte)
        Public Bits As BitField8

        ''' <summary> Bit 0: Grayscale (0: normal color, 1: monochrome) </summary>
        Public Property Grayscale As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(0)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(0, value)
            End Set
        End Property

        ''' <summary> Bit 1: Show background in leftmost 8 pixels of screen </summary>
        Public Property RenderBackgroundLeft As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(1)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(1, value)
            End Set
        End Property

        ''' <summary> Bit 2: Show sprites in leftmost 8 pixels of screen </summary>
        Public Property RenderSpritesLeft As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(2)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(2, value)
            End Set
        End Property

        ''' <summary> Bit 3: Show background </summary>
        Public Property RenderBackground As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(3)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(3, value)
            End Set
        End Property

        ''' <summary> Bit 4: Show sprites </summary>
        Public Property RenderSprites As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(4)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(4, value)
            End Set
        End Property

        ''' <summary> Bit 5: Emphasize Red </summary>
        Public Property EnhanceRed As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(5)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(5, value)
            End Set
        End Property

        ''' <summary> Bit 6: Emphasize Green </summary>
        Public Property EnhanceGreen As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(6)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(6, value)
            End Set
        End Property

        ''' <summary> Bit 7: Emphasize Blue </summary>
        Public Property EnhanceBlue As Boolean
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return Bits.GetBit(7)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Boolean)
                Bits.WriteBit(7, value)
            End Set
        End Property

        ''' <summary> The raw register value (mimics the 'reg' union member) </summary>
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