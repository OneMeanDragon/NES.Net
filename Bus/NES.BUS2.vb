Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports Nintendo.Nintendo.NintendoEntertainmentSystem

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
        Public ReadOnly CPU As New CPU6502() 'CPU6502() 'em6502()
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
        Public AudioSampleReady As Boolean
        Private _audioSample As Double
        Private _audioTime As Double
        Private _audioTimePerNESClock As Double
        Private _audioTimePerSystemSample As Double
        Private _audioSampleCounter As Integer = 0  ' DEBUG counter

#Region "Audio System"
        'Private ReadOnly _audioSystem As New Core.Audio.AudioSystem()
        Private ReadOnly _audioSystem As New CallbackAudioSystem()

        Public ReadOnly Property AudioSystem As CallbackAudioSystem
            Get
                Return _audioSystem
            End Get
        End Property

        ' Add a circular buffer for samples
        Private Const AUDIO_RINGBUFFER_SIZE As UInteger = 41983 '8191  ' Power of 2 minus 1
        Private _audioBuffer(AUDIO_RINGBUFFER_SIZE) As Double
        Private _audioBufferWrite As Integer = 0
        Private _audioBufferRead As Integer = 0
        Private _audioBufferLock As New Object()
        Private _lastValidSample As Double = 0.0  ' Hold last valid sample
        Private _bufferUnderrunCount As Long = 0

        ''' <summary>
        ''' Get number of samples currently in the ring buffer
        ''' </summary>
        Public ReadOnly Property AudioBufferLevel As Integer
            Get
                SyncLock _audioBufferLock
                    Dim diff = _audioBufferWrite - _audioBufferRead
                    If diff < 0 Then diff += (AUDIO_RINGBUFFER_SIZE + 1)
                    Return diff
                End SyncLock
            End Get
        End Property

        ''' <summary>
        ''' Audio callback - called by audio system when it needs samples
        ''' </summary>
        Private Function GetAudioSample(sampleIndex As Long, time As Double) As Double
            SyncLock _audioBufferLock
                ' Check if buffer has samples
                Dim available = _audioBufferWrite - _audioBufferRead
                If available < 0 Then available += (AUDIO_RINGBUFFER_SIZE + 1)

                If available > 0 Then
                    ' Get sample from buffer
                    Dim sample = _audioBuffer(_audioBufferRead)
                    _audioBufferRead = (_audioBufferRead + 1) And AUDIO_RINGBUFFER_SIZE

                    ' Clamp to valid range (critical!)
                    sample = Math.Max(-1.0, Math.Min(1.0, sample))

                    ' Store as last valid sample
                    _lastValidSample = sample
                    Return sample
                Else
                    ' Buffer underrun - return last valid sample (not silence!)
                    _bufferUnderrunCount += 1

                    ' Debug every 10000 underruns
                    If (_bufferUnderrunCount Mod 10000) = 0 Then
                        Debug.WriteLine($"[Audio] Buffer underrun #{_bufferUnderrunCount}, level={available}")
                    End If

                    Return _lastValidSample  ' Hold last sample instead of silence
                End If
            End SyncLock
        End Function
#End Region
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

            ' Setup audio timing
            SetSampleFrequency(AUDIO_SAMPLE_RATE)

            ' Initialize audio system
            'If Not _audioSystem.Initialize(AUDIO_SAMPLE_RATE) Then
            '    Debug.WriteLine("[Bus] Warning: Audio system failed to initialize")
            'End If
            If Not _audioSystem.Initialize(44100, 1, 16, 512, AddressOf GetAudioSample) Then
                Debug.WriteLine("[Bus] Warning: Audio system failed to initialize")
            End If
            For i = 0 To 2047
                _audioBuffer(i) = 0.0
            Next
            _audioBufferWrite = 2048

            ' Initialize state (reminder to reset at some point)
            ' Reset()
        End Sub


        Public Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                _audioSystem?.Dispose()
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

            Debug.WriteLine($"[Bus] Audio timing: {_audioTimePerNESClock:E6}s per clock, {_audioTimePerSystemSample:E6}s per sample")
            Debug.WriteLine($"[Bus] Expected samples per frame: {NES_MASTER_CLOCK / sampleRate / 60.0:F2}")
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
                'Console.WriteLine($"*** DMA TRIGGERED from page ${data:X2} (base address ${data:X2}00) ***")
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
            _audioSampleCounter = 0

            ' Clear audio buffer
            SyncLock _audioBufferLock
                Array.Clear(_audioBuffer, 0, _audioBuffer.Length)
                _audioBufferWrite = 0
                _audioBufferRead = 0
                _lastValidSample = 0.0
                _bufferUnderrunCount = 0
            End SyncLock
            _audioSystem?.Reset()  ' Reset the audio system too!

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

            ' CPU runs at 1/3 PPU speed _dmaDummy
            If (_systemClockCounter Mod 3) = 0 Then
                If _dmaTransfer Then ' DMA in progress - CPU is halted
                    ProcessDMA()
                Else ' Normal CPU operation
                    CPU.Clock()
                End If
            End If

            ' APU Handle audio timing
            AudioSampleReady = ProcessAudio()
            'If AudioSampleReady Then
            '    _audioSystem.ProcessSample(_audioSample)
            'End If
            If AudioSampleReady Then
                ' Check if APU sample is valid
                Dim sample = _audioSample

                ' Clamp sample to valid range BEFORE adding to buffer
                sample = Math.Max(-1.0, Math.Min(1.0, sample))

                ' Filter out NaN and Infinity
                If Double.IsNaN(sample) OrElse Double.IsInfinity(sample) Then
                    sample = 0.0
                    Debug.WriteLine("[Audio] Invalid sample detected (NaN/Inf)")
                End If

                SyncLock _audioBufferLock
                    ' Check if buffer has room
                    Dim nextWrite = (_audioBufferWrite + 1) And AUDIO_RINGBUFFER_SIZE
                    If nextWrite <> _audioBufferRead Then
                        ' Room available - add sample
                        _audioBuffer(_audioBufferWrite) = sample
                        _audioBufferWrite = nextWrite
                    Else
                        ' Buffer full - skip this sample (overflow)
                        ' This is rare and better than distortion
                    End If
                End SyncLock
            End If

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

            Return AudioSampleReady
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
                Dim addr = CUShort((CUShort(_dmaPage) << 8) Or _dmaAddr)
                _dmaData = CpuRead(addr)

                ' DEBUG: Print first few reads
                'If _dmaAddr < 16 Then
                '    Console.WriteLine($"DMA read: page=${_dmaPage:X2}, addr=${_dmaAddr:X2}, full=${addr:X4} = ${_dmaData:X2}")
                'End If
            Else
                ' Odd cycle: Write to OAM
                'Dim oamIndex = _dmaAddr >> 2  ' Divide by 4 to get sprite index (0-63)
                'If oamIndex < 64 Then
                '    Dim byteIndex = _dmaAddr And 3  ' Get byte within sprite (0-3)
                '    Select Case byteIndex
                '        Case 0 : PPU.OAM(oamIndex).Y = _dmaData
                '        Case 1 : PPU.OAM(oamIndex).TileID = _dmaData
                '        Case 2 : PPU.OAM(oamIndex).Attributes = _dmaData
                '        Case 3 : PPU.OAM(oamIndex).X = _dmaData
                '    End Select
                'End If

                Dim oamIndex = _dmaAddr >> 2  ' Same as _dmaAddr \ 4 but MUCH faster
                ' Your OAMEntry.SetByteAt already masks byteIndex with &H3, so this is safe!
                If oamIndex < 64 Then
                    PPU.OAM(oamIndex).SetByteAt(_dmaAddr, _dmaData)  ' Pass full address, it masks internally
                End If

                ' Increment address after write
                _dmaAddr = CByte((_dmaAddr + 1) And &HFF)

                ' Check if DMA complete
                If _dmaAddr = 0 Then
                    _dmaTransfer = False
                    _dmaDummy = True
                    'Console.WriteLine($"DMA complete! First sprite: Y={PPU.OAM(0).Y}, Tile=${PPU.OAM(0).TileID:X2}, X={PPU.OAM(0).X}")
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

                ' Get sample from APU
                _audioSample = APU.GetOutputSample()

                ' Debug counter
                _audioSampleCounter += 1

                Return True
            End If

            Return False
        End Function
        ' Add this diagnostic method to check sample generation
        Public Function GetAudioSampleRate() As Double
            ' Calculate actual sample generation rate
            Dim rate = _audioSampleCounter / (_systemClockCounter / NES_MASTER_CLOCK)
            Return rate
        End Function

#End Region

    End Class

End Namespace