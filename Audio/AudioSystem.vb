Imports System.Runtime.CompilerServices
Imports NAudio.Wave

Namespace Core.Audio

    ''' <summary>
    ''' High-performance audio system for NES emulator
    ''' Handles audio buffering, mixing, and output via NAudio
    ''' </summary>
    Public NotInheritable Class AudioSystem
        Implements IDisposable

#Region "Constants"
        Private Const SAMPLE_RATE As Integer = 44100        ' CD quality
        Private Const BUFFER_SIZE As Integer = 176400       ' 4 seconds buffer (increased)
        Private Const BATCH_SIZE As Integer = 4096          ' Larger batches (increased from 2048)
        Private Const MIN_BUFFER_MS As Integer = 100        ' More buffer before playback (increased from 50)
        Private Const SILENCE_THRESHOLD As Double = 0.01    ' Threshold for silence detection
        Private Const MAX_SAMPLE_JUMP As Double = 0.3       ' Maximum allowed sample-to-sample change (anti-pop)
#End Region

#Region "NAudio Components"
        Private _waveOut As WaveOutEvent
        Private _audioProvider As BufferedWaveProvider
        Private _waveFormat As WaveFormat
#End Region

#Region "Buffering"
        Private _sampleBatch As New List(Of Byte)(BATCH_SIZE * 2)
        Private _isInitialized As Boolean = False
        Private _isDisposed As Boolean = False
        Private _lastSampleValue As Double = 0.0  ' For smoothing

        ' Low-pass filter state (simple RC filter)
        Private _lpfEnabled As Boolean = True
        Private _lpfAlpha As Double = 0.85  ' Filter coefficient (higher = more filtering)
        Private _lpfPrevSample As Double = 0.0
#End Region

#Region "Statistics (for debugging)"
        Private _totalSamplesProcessed As Long = 0
        Private _silentSamplesFiltered As Long = 0
        Private _bufferOverflows As Long = 0
#End Region

#Region "Properties"
        ''' <summary>
        ''' Check if audio system is initialized and ready
        ''' </summary>
        Public ReadOnly Property IsInitialized As Boolean
            Get
                Return _isInitialized
            End Get
        End Property

        ''' <summary>
        ''' Get current playback state
        ''' </summary>
        Public ReadOnly Property PlaybackState As PlaybackState
            Get
                If _waveOut Is Nothing Then Return NAudio.Wave.PlaybackState.Stopped
                Return _waveOut.PlaybackState
            End Get
        End Property

        ''' <summary>
        ''' Get/Set output volume (0.0 to 1.0)
        ''' </summary>
        Public Property Volume As Single
            Get
                If _waveOut Is Nothing Then Return 0.0F
                Return _waveOut.Volume
            End Get
            Set(value As Single)
                If _waveOut IsNot Nothing Then
                    _waveOut.Volume = Math.Max(0.0F, Math.Min(1.0F, value))
                End If
            End Set
        End Property

        ''' <summary>
        ''' Enable/disable low-pass filter for smoother audio
        ''' </summary>
        Public Property LowPassFilterEnabled As Boolean
            Get
                Return _lpfEnabled
            End Get
            Set(value As Boolean)
                _lpfEnabled = value
                If Not value Then _lpfPrevSample = 0.0
            End Set
        End Property

        ''' <summary>
        ''' Low-pass filter strength (0.0 = no filter, 0.95 = very strong)
        ''' </summary>
        Public Property LowPassFilterStrength As Double
            Get
                Return _lpfAlpha
            End Get
            Set(value As Double)
                _lpfAlpha = Math.Max(0.0, Math.Min(0.95, value))
            End Set
        End Property

        ''' <summary>
        ''' Get current buffer duration in milliseconds
        ''' </summary>
        Public ReadOnly Property BufferedMilliseconds As Double
            Get
                If _audioProvider Is Nothing Then Return 0.0
                Return _audioProvider.BufferedDuration.TotalMilliseconds
            End Get
        End Property

        ''' <summary>
        ''' Get statistics about audio processing
        ''' </summary>
        Public ReadOnly Property Statistics As AudioStatistics
            Get
                Return New AudioStatistics With {
                    .TotalSamplesProcessed = _totalSamplesProcessed,
                    .SilentSamplesFiltered = _silentSamplesFiltered,
                    .BufferOverflows = _bufferOverflows,
                    .CurrentBufferMs = BufferedMilliseconds
                }
            End Get
        End Property
#End Region

#Region "Initialization"
        Public Sub New()
            ' Constructor - call Initialize() separately for better error handling
        End Sub

        'InitialiseAudio(
        '   unsigned int nSampleRate = SAMPLE_RATE,
        '   unsigned int nChannels = 1,
        '   unsigned int nBlocks = 8,
        '   unsigned int nBlockSamples = 512
        ')
        '
        ''' <summary>
        ''' Initialize the audio system
        ''' </summary>
        Public Function Initialize(Optional sampleRate As Integer = SAMPLE_RATE) As Boolean
            If _isInitialized Then Return True

            Try
                ' Setup wave format (16-bit mono PCM)
                _waveFormat = New WaveFormat(sampleRate, 16, 1) '16, 1)

                ' Create buffered provider
                _audioProvider = New BufferedWaveProvider(_waveFormat) With {
                    .BufferLength = BUFFER_SIZE,
                    .DiscardOnBufferOverflow = True
                }

                ' Create wave output
                _waveOut = New WaveOutEvent() With {
                    .DeviceNumber = -1,  ' Default device
                    .DesiredLatency = 150,  ' Increased latency for smoother playback
                    .NumberOfBuffers = 3    ' More buffers to prevent underruns
                }

                _waveOut.Init(_audioProvider)
                _waveOut.Volume = 0.5F  ' Start at 50% volume

                _isInitialized = True
                Debug.WriteLine($"[Audio] Initialized successfully - {sampleRate}Hz, 16-bit mono")
                Return True

            Catch ex As Exception
                Debug.WriteLine($"[Audio] Initialization failed: {ex.Message}")
                Cleanup()
                Return False
            End Try
        End Function
#End Region

#Region "Core Audio Processing"
        ''' <summary>
        ''' Process a single audio sample from the NES APU
        ''' Call this every time the emulator produces a sample
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub ProcessSample(sample As Double)
            If Not _isInitialized Then Return

            _totalSamplesProcessed += 1

            ' Apply silence filter (NES APU often outputs a specific DC offset)
            If Math.Abs(sample + 0.26) < SILENCE_THRESHOLD Then
                sample = 0.0
                _silentSamplesFiltered += 1
            End If

            ' Apply low-pass filter to remove high-frequency noise
            If _lpfEnabled Then
                sample = (_lpfAlpha * _lpfPrevSample) + ((1.0 - _lpfAlpha) * sample)
                _lpfPrevSample = sample
            End If

            ' Anti-pop filter: Limit sample-to-sample jumps to prevent clicks/pops
            Dim delta = sample - _lastSampleValue
            If Math.Abs(delta) > MAX_SAMPLE_JUMP Then
                ' Smooth the transition instead of jumping
                sample = _lastSampleValue + Math.Sign(delta) * MAX_SAMPLE_JUMP
            End If
            _lastSampleValue = sample

            ' Clamp sample to valid range
            sample = Math.Max(-1.0, Math.Min(1.0, sample))

            ' Convert to 16-bit PCM
            Dim sample16 As Int16 = CShort(sample * 32767.0)
            Dim bytes() As Byte = BitConverter.GetBytes(sample16)

            ' Add to batch
            _sampleBatch.AddRange(bytes)

            ' Flush batch when it reaches the threshold
            If _sampleBatch.Count >= BATCH_SIZE Then
                FlushBatch()
            End If
        End Sub

        ''' <summary>
        ''' Flush the current sample batch to the audio provider
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub FlushBatch()
            If _sampleBatch.Count = 0 Then Return

            Try
                ' Check if there's room in the buffer
                Dim availableSpace = _audioProvider.BufferLength - _audioProvider.BufferedBytes

                If availableSpace < _sampleBatch.Count Then
                    _bufferOverflows += 1

                    ' If buffer is full, skip some old data to make room
                    ' This prevents audio from getting too far behind
                    If _audioProvider.BufferedBytes > (_audioProvider.BufferLength * 0.9) Then
                        _audioProvider.ClearBuffer()
                        Debug.WriteLine("[Audio] Buffer cleared due to overflow")
                    End If
                End If

                ' Add samples to provider
                If availableSpace >= _sampleBatch.Count Then
                    _audioProvider.AddSamples(_sampleBatch.ToArray(), 0, _sampleBatch.Count)
                End If

                ' Auto-start playback if we have enough buffer and not playing
                If _waveOut.PlaybackState <> NAudio.Wave.PlaybackState.Playing AndAlso
                   _audioProvider.BufferedDuration.TotalMilliseconds >= MIN_BUFFER_MS Then
                    _waveOut.Play()
                    Debug.WriteLine("[Audio] Auto-started playback")
                End If

            Catch ex As Exception
                Debug.WriteLine($"[Audio] Error flushing batch: {ex.Message}")
            Finally
                _sampleBatch.Clear()
            End Try
        End Sub

        ''' <summary>
        ''' Force flush any remaining samples in the batch
        ''' </summary>
        Public Sub Flush()
            FlushBatch()
        End Sub
#End Region

#Region "Playback Control"
        ''' <summary>
        ''' Start audio playback
        ''' </summary>
        Public Sub Play()
            If Not _isInitialized Then Return
            If _waveOut.PlaybackState <> NAudio.Wave.PlaybackState.Playing Then
                _waveOut.Play()
                Debug.WriteLine("[Audio] Playback started")
            End If
        End Sub

        ''' <summary>
        ''' Pause audio playback
        ''' </summary>
        Public Sub Pause()
            If Not _isInitialized Then Return
            If _waveOut.PlaybackState = NAudio.Wave.PlaybackState.Playing Then
                _waveOut.Pause()
                Debug.WriteLine("[Audio] Playback paused")
            End If
        End Sub

        ''' <summary>
        ''' Stop audio playback and clear buffers
        ''' </summary>
        Public Sub [Stop]()
            If Not _isInitialized Then Return
            _waveOut?.Stop()
            _audioProvider?.ClearBuffer()
            _sampleBatch.Clear()
            Debug.WriteLine("[Audio] Playback stopped")
        End Sub

        ''' <summary>
        ''' Reset audio system (clears buffers but keeps initialized)
        ''' </summary>
        Public Sub Reset()
            If Not _isInitialized Then Return
            [Stop]()
            _totalSamplesProcessed = 0
            _silentSamplesFiltered = 0
            _bufferOverflows = 0
            _lastSampleValue = 0.0
            Debug.WriteLine("[Audio] System reset")
        End Sub
#End Region

#Region "Cleanup"
        Private Sub Cleanup()
            Try
                _waveOut?.Stop()
                _waveOut?.Dispose()
                _waveOut = Nothing
                _audioProvider = Nothing
                _sampleBatch?.Clear()
                _isInitialized = False
            Catch ex As Exception
                Debug.WriteLine($"[Audio] Cleanup error: {ex.Message}")
            End Try
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                Cleanup()
                _isDisposed = True
                Debug.WriteLine("[Audio] Disposed")
            End If
        End Sub
#End Region

    End Class

    ''' <summary>
    ''' Audio statistics for monitoring and debugging
    ''' </summary>
    Public Structure AudioStatistics
        Public TotalSamplesProcessed As Long
        Public SilentSamplesFiltered As Long
        Public BufferOverflows As Long
        Public CurrentBufferMs As Double

        Public Overrides Function ToString() As String
            Return $"Samples: {TotalSamplesProcessed:N0}, Silent: {SilentSamplesFiltered:N0}, Overflows: {BufferOverflows}, Buffer: {CurrentBufferMs:F1}ms"
        End Function
    End Structure

End Namespace