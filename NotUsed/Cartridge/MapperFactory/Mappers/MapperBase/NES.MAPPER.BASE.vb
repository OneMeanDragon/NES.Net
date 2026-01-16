Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Mirroring modes for nametable arrangement
    ''' </summary>
    Public Enum [MirrorMode] As Byte
        Hardware = 0        ' Determined by cart hardware
        Horizontal = 1      ' Vertical arrangement
        Vertical = 2        ' Horizontal arrangement  
        OneScreenLo = 3     ' Single screen, lower bank
        OneScreenHi = 4     ' Single screen, upper bank
        FourScreen = 5      ' Four-screen (extra VRAM)
    End Enum

    ''' <summary>
    ''' Modern, high-performance base mapper class using aggressive inlining
    ''' </summary>
    Public MustInherit Class MapperBase
        Implements IDisposable

        ' Core properties
        Protected ReadOnly _prgBanks As Byte
        Protected ReadOnly _chrBanks As Byte
        Protected _mirrorMode As MirrorMode
        Protected _isDisposed As Boolean = False

        ' Optional cartridge RAM (for save games, work RAM)
        Protected _cartRam As Memory(Of Byte)

        Public ReadOnly Property PrgBanks As Byte
            Get
                Return _prgBanks
            End Get
        End Property

        Public ReadOnly Property ChrBanks As Byte
            Get
                Return _chrBanks
            End Get
        End Property

        Public MustOverride ReadOnly Property MapperName As String
        Public MustOverride ReadOnly Property MapperNumber As Byte

        Protected Sub New(prgBanks As Byte, chrBanks As Byte)
            _prgBanks = prgBanks
            _chrBanks = chrBanks
            _mirrorMode = MirrorMode.Hardware
        End Sub

        ' CPU bus interface - must be fast
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public MustOverride Function CpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32, ByRef data As Byte) As Boolean

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public MustOverride Function CpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32, data As Byte) As Boolean

        ' PPU bus interface - must be fast
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public MustOverride Function PpuMapRead(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public MustOverride Function PpuMapWrite(addr As UInt16, ByRef mappedAddr As UInt32) As Boolean

        ' Reset mapper state
        Public MustOverride Sub Reset()

        ' Get current mirror mode
        Public Overridable Function GetMirrorMode() As MirrorMode
            Return _mirrorMode
        End Function

        ' IRQ support (for advanced mappers like MMC3)
        Public Overridable Function IsIrqActive() As Boolean
            Return False
        End Function

        Public Overridable Sub ClearIrq()
        End Sub

        Public Overridable Sub ScanlineCounter()
        End Sub

        Public Overridable Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                _cartRam = Nothing
                _isDisposed = True
            End If
        End Sub

    End Class

End Namespace