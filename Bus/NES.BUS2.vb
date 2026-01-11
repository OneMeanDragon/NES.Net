Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' Modern, high-performance NES system bus connecting all components
    ''' </summary>
    Public NotInheritable Class NESBus
        Implements IDisposable

#Region "Constants"
        Private Const CPU_RAM_SIZE As Integer = 2048       ' 2KB CPU RAM
        Private Const CPU_RAM_MIRROR_MASK As UInt16 = &H7FFUS ' Mirrors every 2KB in $0000-$1FFF
        Private Const PPU_REG_MIRROR_MASK As UInt16 = &H7US   ' 8 PPU registers mirrored
        Private Const AUDIO_SAMPLE_RATE As UInt32 = 44100
        Private Const NES_MASTER_CLOCK As Double = 5369318.0  ' NTSC master clock (PPU clock rate)
#End Region

#Region "Components"
        ' Core NES components
        Public ReadOnly CPU As New em6502() 'CPU6502() 'em6502()
        Public ReadOnly PPU As New PPU2C02() 'em2C02()
        Public ReadOnly APU As New em2A03()

        ' Memory
        Private ReadOnly _cpuRam As Memory(Of Byte)

        ' Controllers (using modern input handling)
        Private _controllerState(1) As Byte
        Private _controllerLatch(1) As Byte
#End Region

#Region "DMA State"
        Private _dmaPage As Byte
        Private _dmaAddr As Byte
        Private _dmaData As Byte
        Private _dmaDummy As Boolean
        Private _dmaTransfer As Boolean
#End Region

#Region "Audio Timing"
        Private _audioSample As Double
        Private _audioTime As Double
        Private _audioTimePerNESClock As Double
        Private _audioTimePerSystemSample As Double
#End Region

#Region "System State"
        Private _systemClockCounter As UInt64
        Private _isDisposed As Boolean
#End Region

#Region "Properties"
        Public ReadOnly Property SystemClockCount As UInt64
            Get
                Return _systemClockCounter
            End Get
        End Property

        Public ReadOnly Property AudioSample As Double
            Get
                Return _audioSample
            End Get
        End Property

        ''' <summary>
        ''' Get/Set controller button states (8 buttons per controller)
        ''' Bit 0=A, 1=B, 2=Select, 3=Start, 4=Up, 5=Down, 6=Left, 7=Right
        ''' </summary>
        Public Property Controller(index As Integer) As Byte
            Get
                If index >= 0 AndAlso index <= 1 Then
                    Return _controllerLatch(index)
                End If
                Return 0
            End Get
            Set(value As Byte)
                If index >= 0 AndAlso index <= 1 Then
                    _controllerLatch(index) = value
                End If
            End Set
        End Property
#End Region

#Region "Constructor / Destructor"
        Public Sub New()
            ' Initialize CPU RAM
            _cpuRam = New Memory(Of Byte)(New Byte(CPU_RAM_SIZE - 1) {})

            ' Connect CPU to this bus
            CPU.ConnectBus(Me)

            ' Setup audio timing
            SetSampleFrequency(AUDIO_SAMPLE_RATE)

            ' Initialize state (reminder to reset at some point)
            ' Reset()
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                _isDisposed = True
            End If
        End Sub
#End Region

#Region "Configuration"
        ''' <summary>
        ''' Configure audio sample rate (call before running emulation)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetSampleFrequency(sampleRate As UInt32)
            _audioTimePerSystemSample = 1.0 / sampleRate
            _audioTimePerNESClock = 1.0 / NES_MASTER_CLOCK
        End Sub
#End Region

#Region "CPU Bus Interface"
        ''' <summary>
        ''' CPU read from system bus (aggressively inlined for performance)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function CpuRead(addr As UInt16, Optional isReadOnly As Boolean = False) As Byte
            Dim data As Byte = 0

            ' Try cartridge first (it can override everything)
            If Cart.CpuRead(addr, data) Then
                Return data
            End If

            ' CPU RAM ($0000-$1FFF, mirrored every 2KB)
            If addr <= &H1FFFUS Then
                Return _cpuRam.Span(addr And CPU_RAM_MIRROR_MASK)
            End If

            ' PPU Registers ($2000-$3FFF, mirrored every 8 bytes)
            If addr >= &H2000US AndAlso addr <= &H3FFFUS Then
                Return PPU.cpuRead(addr And PPU_REG_MIRROR_MASK, isReadOnly)
            End If

            ' APU Status ($4015)
            If addr = &H4015US Then
                Return APU.CpuRead(addr)
            End If

            ' Controller reads ($4016-$4017)
            If addr >= &H4016US AndAlso addr <= &H4017US Then
                Dim controllerIndex = addr And 1
                data = If((_controllerState(controllerIndex) And &H80) <> 0, 1, 0)
                _controllerState(controllerIndex) <<= 1
                Return data
            End If

            ' Open bus (return 0)
            Return 0
        End Function

        ''' <summary>
        ''' CPU write to system bus (aggressively inlined for performance)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub CpuWrite(addr As UInt16, data As Byte)
            ' Try cartridge first
            If Cart.CpuWrite(addr, data) Then
                Return
            End If

            ' CPU RAM ($0000-$1FFF, mirrored every 2KB)
            If addr <= &H1FFFUS Then
                Dim span = _cpuRam.Span
                span(addr And CPU_RAM_MIRROR_MASK) = data
                Return
            End If

            ' PPU Registers ($2000-$3FFF, mirrored every 8 bytes)
            If addr >= &H2000US AndAlso addr <= &H3FFFUS Then
                PPU.cpuWrite(addr And PPU_REG_MIRROR_MASK, data)
                Return
            End If

            ' APU and I/O registers ($4000-$4017)
            If addr >= &H4000US AndAlso addr <= &H4013US Then
                APU.CpuWrite(addr, data)
                Return
            End If

            If addr = &H4014US Then
                ' OAM DMA
                _dmaPage = data
                _dmaAddr = 0
                _dmaTransfer = True
                _dmaDummy = True
                Return
            End If

            If addr = &H4015US OrElse addr = &H4017US Then
                APU.CpuWrite(addr, data)
                Return
            End If

            ' Controller strobe ($4016-$4017)
            If addr >= &H4016US AndAlso addr <= &H4017US Then
                Dim controllerIndex = addr And 1
                _controllerState(controllerIndex) = _controllerLatch(controllerIndex)
                Return
            End If
        End Sub
#End Region

#Region "Reset"
        ''' <summary>
        ''' Reset the entire system
        ''' </summary>
        Public Sub Reset()
            ' Reset cartridge (if loaded)
            Cart?.Reset()

            ' Reset components
            CPU.Reset()
            PPU.Reset()
            APU.Reset()

            ' Clear RAM (actual NES has random values, but zeros are fine)
            _cpuRam.Span.Clear()

            ' Reset DMA
            _dmaPage = 0
            _dmaAddr = 0
            _dmaData = 0
            _dmaDummy = True
            _dmaTransfer = False

            ' Reset audio
            _audioSample = 0.0
            _audioTime = 0.0

            ' Reset clock
            _systemClockCounter = 0

            ' Reset controllers
            _controllerState(0) = 0
            _controllerState(1) = 0
            _controllerLatch(0) = 0
            _controllerLatch(1) = 0
        End Sub
#End Region

#Region "Main Clock"
        ' Clock the entire system one PPU cycle
        ' Returns True when an audio sample is ready
        '<MethodImpl(MethodImplOptions.AggressiveOptimization)>
        Public Function Clock() As Boolean
            ' Clock PPU (runs every cycle)
            PPU.Clock()

            ' Clock APU (runs every cycle)
            APU.Clock()

            ' CPU runs at 1/3 PPU speed
            If (_systemClockCounter Mod 3) = 0 Then
                If _dmaTransfer Then
                    ' DMA in progress - CPU is halted
                    ProcessDMA()
                Else
                    ' Normal CPU operation
                    CPU.Clock()
                End If
            End If

            ' Handle audio timing
            Dim audioReady = ProcessAudio()

            ' Handle NMI from PPU
            If PPU.NmiRequested Then
                PPU.NmiRequested = False
                CPU.NMI()
            End If

            ' Handle IRQ from cartridge (mapper-based)
            If Cart?.Mapper IsNot Nothing AndAlso Cart.Mapper.IsIrqActive() Then
                Cart.Mapper.ClearIrq()
                CPU.IRQ()
            End If

            ' Increment system clock
            _systemClockCounter += 1

            Return audioReady
        End Function

        ''' <summary>
        ''' Process DMA transfer (inlined for performance)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub ProcessDMA()
            ' Wait for even cycle to start
            If _dmaDummy Then
                If (_systemClockCounter Mod 2) = 1 Then
                    _dmaDummy = False
                End If
                Return
            End If

            ' Perform DMA transfer
            If (_systemClockCounter Mod 2) = 0 Then
                ' Even cycle: Read from CPU memory
                Dim addr = CUShort((_dmaPage << 8) Or _dmaAddr)
                _dmaData = CpuRead(addr)
            Else
                ' Odd cycle: Write to OAM
                Dim oamIndex = _dmaAddr \ 4
                Dim byteOffset = _dmaAddr Mod 4

                If oamIndex < 64 Then
                    PPU.OAM(oamIndex).SetByteAt(byteOffset, _dmaData)
                End If

                _dmaAddr = If(_dmaAddr = 255, 0, _dmaAddr + 1)

                ' Check if DMA complete
                If _dmaAddr = 0 Then
                    _dmaTransfer = False
                    _dmaDummy = True
                End If
            End If
        End Sub

        ''' <summary>
        ''' Process audio timing and sample generation
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Function ProcessAudio() As Boolean
            _audioTime += _audioTimePerNESClock

            If _audioTime >= _audioTimePerSystemSample Then
                _audioTime -= _audioTimePerSystemSample
                _audioSample = APU.GetOutputSample()
                Return True
            End If

            Return False
        End Function
#End Region

    End Class

End Namespace