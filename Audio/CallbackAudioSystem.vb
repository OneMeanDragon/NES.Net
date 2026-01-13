Imports System.Runtime.CompilerServices
Imports NAudio.Wave

Namespace Nintendo.NintendoEntertainmentSystem

    ''' <summary>
    ''' Callback-based audio system inspired by olcPixelGameEngine
    ''' Uses a pull model where audio system requests samples via callback
    ''' Similar to: olc::SOUND::InitialiseAudio(44100, 1, 8, 512)
    ''' </summary>
    Public NotInheritable Class CallbackAudioSystem
        Implements IDisposable

#Region "Callback Delegate"
        ''' <summary>
        ''' Callback function that provides audio samples on demand
        ''' Returns a sample value between -1.0 and 1.0
        ''' Parameters: sampleIndex, time
        ''' </summary>
        Public Delegate Function AudioSampleCallback(sampleIndex As Long, time As Double) As Double
#End Region

#Region "Components"
        Private _waveOut As WaveOutEvent
        Private _waveProvider As CallbackWaveProvider
        Private _isInitialized As Boolean = False
        Private _isDisposed As Boolean = False
#End Region

#Region "Configuration"
        Private _sampleRate As Integer = 44100
        Private _channels As Integer = 1
        Private _bitsPerSample As Integer = 16
        Private _blockSize As Integer = 512
#End Region

#Region "Statistics"
        Private _totalSamplesGenerated As Long = 0
        Private _currentTime As Double = 0.0
#End Region

#Region "Properties"
        Public ReadOnly Property IsInitialized As Boolean
            Get
                Return _isInitialized
            End Get
        End Property

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

        Public ReadOnly Property SamplesGenerated As Long
            Get
                Return _totalSamplesGenerated
            End Get
        End Property
#End Region

#Region "Initialization"
        ''' <summary>
        ''' Initialize audio system with callback
        ''' Similar to: olc::SOUND::InitialiseAudio(44100, 1, 8, 512)
        ''' </summary>
        ''' <param name="sampleRate">Sample rate (typically 44100 or 48000)</param>
        ''' <param name="channels">Number of channels (1=mono, 2=stereo)</param>
        ''' <param name="bitsPerSample">Bits per sample (8 or 16)</param>
        ''' <param name="blockSize">Buffer block size (samples per block, e.g. 512)</param>
        ''' <param name="callback">Function to call for generating samples</param>
        Public Function Initialize(sampleRate As Integer,
                                  channels As Integer,
                                  bitsPerSample As Integer,
                                  blockSize As Integer,
                                  callback As AudioSampleCallback) As Boolean
            If _isInitialized Then Return True

            Try
                _sampleRate = sampleRate
                _channels = channels
                _bitsPerSample = bitsPerSample
                _blockSize = blockSize

                ' Create wave format
                Dim format As New WaveFormat(sampleRate, bitsPerSample, channels)

                ' Create callback provider
                _waveProvider = New CallbackWaveProvider(format, callback)

                ' Create wave output
                _waveOut = New WaveOutEvent() With {
                    .DeviceNumber = -1,
                    .DesiredLatency = blockSize * 1000 \ sampleRate  ' Calculate latency from block size
                }

                _waveOut.Init(_waveProvider)
                _waveOut.Volume = 0.5F

                _isInitialized = True
                Debug.WriteLine($"[Audio] Initialized: {sampleRate}Hz, {channels}ch, {bitsPerSample}bit, block={blockSize}")
                Return True

            Catch ex As Exception
                Debug.WriteLine($"[Audio] Initialization failed: {ex.Message}")
                Cleanup()
                Return False
            End Try
        End Function
#End Region

#Region "Playback Control"
        Public Sub Play()
            If Not _isInitialized Then Return
            If _waveOut.PlaybackState <> PlaybackState.Playing Then
                _waveOut.Play()
                Debug.WriteLine("[Audio] Started")
            End If
        End Sub

        Public Sub Pause()
            If Not _isInitialized Then Return
            If _waveOut.PlaybackState = PlaybackState.Playing Then
                _waveOut.Pause()
                Debug.WriteLine("[Audio] Paused")
            End If
        End Sub

        Public Sub [Stop]()
            If Not _isInitialized Then Return
            _waveOut?.Stop()
            _totalSamplesGenerated = 0
            _currentTime = 0.0
            Debug.WriteLine("[Audio] Stopped")
        End Sub

        Public Sub Reset()
            If Not _isInitialized Then Return
            [Stop]()
            Debug.WriteLine("[Audio] Reset")
        End Sub
#End Region

#Region "Cleanup"
        Private Sub Cleanup()
            Try
                _waveOut?.Stop()
                _waveOut?.Dispose()
                _waveOut = Nothing
                _waveProvider = Nothing
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

#Region "Callback Wave Provider"
        ''' <summary>
        ''' Wave provider that generates audio via callback
        ''' </summary>
        Private Class CallbackWaveProvider
            Implements IWaveProvider

            Private ReadOnly _format As WaveFormat
            Private ReadOnly _callback As AudioSampleCallback
            Private _sampleIndex As Long = 0
            Private _time As Double = 0.0

            ' Low-pass filter state
            Private _lpfAlpha As Double = 0.85
            Private _lpfPrevSample As Double = 0.0

            Public Sub New(format As WaveFormat, callback As AudioSampleCallback)
                _format = format
                _callback = callback
            End Sub

            Public ReadOnly Property WaveFormat As WaveFormat Implements IWaveProvider.WaveFormat
                Get
                    Return _format
                End Get
            End Property

            Public Function Read(buffer() As Byte, offset As Integer, count As Integer) As Integer Implements IWaveProvider.Read
                Dim bytesPerSample = _format.BitsPerSample \ 8
                Dim samplesNeeded = count \ bytesPerSample

                For i = 0 To samplesNeeded - 1
                    ' Get sample from callback
                    Dim sample = _callback(_sampleIndex, _time)

                    ' Apply low-pass filter
                    sample = (_lpfAlpha * _lpfPrevSample) + ((1.0 - _lpfAlpha) * sample)
                    _lpfPrevSample = sample

                    ' Clamp
                    sample = Math.Max(-1.0, Math.Min(1.0, sample))

                    ' Convert to bytes based on bit depth
                    If _format.BitsPerSample = 16 Then
                        Dim sample16 = CShort(sample * 32767.0)
                        Dim bytes = BitConverter.GetBytes(sample16)
                        ' Copy bytes to output buffer
                        Array.Copy(bytes, 0, buffer, offset + (i * bytesPerSample), bytesPerSample)
                    Else ' 8-bit
                        Dim sample8 = CByte((sample + 1.0) * 127.5)
                        buffer(offset + i) = sample8
                    End If

                    ' Update time and index
                    _sampleIndex += 1
                    _time = _sampleIndex / CDbl(_format.SampleRate)
                Next

                Return count
            End Function

        End Class
#End Region

    End Class

End Namespace