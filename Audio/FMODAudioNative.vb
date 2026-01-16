Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' P/Invoke wrapper for native FMOD audio system
    ''' </summary>
    Public Class FMODAudioNative
        Implements IDisposable

        ' DLL imports
        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateFMODAudio() As IntPtr
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyFMODAudio(audio As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function FMODAudio_Initialize(audio As IntPtr, bus As IntPtr, sampleRate As Integer, bufferSize As Integer) As Boolean
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub FMODAudio_Start(audio As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub FMODAudio_Stop(audio As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub FMODAudio_Pause(audio As IntPtr, pause As Boolean)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub FMODAudio_Update(audio As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub FMODAudio_SetVolume(audio As IntPtr, volume As Single)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function FMODAudio_GetVolume(audio As IntPtr) As Single
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function FMODAudio_IsPlaying(audio As IntPtr) As Boolean
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function FMODAudio_GetLatency(audio As IntPtr) As Integer
        End Function

        ' Instance fields
        Private _audioHandle As IntPtr
        Private _disposed As Boolean = False

        Public Sub New()
            _audioHandle = CreateFMODAudio()
            If _audioHandle = IntPtr.Zero Then
                Throw New Exception("Failed to create FMOD audio system")
            End If
        End Sub

        ''' <summary>
        ''' Initialize FMOD with the NES Bus
        ''' </summary>
        ''' <param name="busHandle">Native Bus handle</param>
        ''' <param name="sampleRate">Audio sample rate (default 44100)</param>
        ''' <param name="bufferSize">Buffer size in samples (default 512 for low latency)</param>
        Public Function Initialize(busHandle As IntPtr, Optional sampleRate As Integer = 44100, Optional bufferSize As Integer = 512) As Boolean
            Return FMODAudio_Initialize(_audioHandle, busHandle, sampleRate, bufferSize)
        End Function

        ''' <summary>
        ''' Start audio playback
        ''' </summary>
        Public Sub Start()
            FMODAudio_Start(_audioHandle)
        End Sub

        ''' <summary>
        ''' Stop audio playback
        ''' </summary>
        Public Sub [Stop]()
            FMODAudio_Stop(_audioHandle)
        End Sub

        ''' <summary>
        ''' Pause or resume audio playback
        ''' </summary>
        Public Sub Pause(pause As Boolean)
            FMODAudio_Pause(_audioHandle, pause)
        End Sub

        ''' <summary>
        ''' Update FMOD (call this once per frame)
        ''' </summary>
        Public Sub Update()
            FMODAudio_Update(_audioHandle)
        End Sub

        ''' <summary>
        ''' Set master volume (0.0 to 1.0)
        ''' </summary>
        Public Property Volume As Single
            Get
                Return FMODAudio_GetVolume(_audioHandle)
            End Get
            Set(value As Single)
                FMODAudio_SetVolume(_audioHandle, value)
            End Set
        End Property

        ''' <summary>
        ''' Check if audio is currently playing
        ''' </summary>
        Public ReadOnly Property IsPlaying As Boolean
            Get
                Return FMODAudio_IsPlaying(_audioHandle)
            End Get
        End Property

        ''' <summary>
        ''' Get approximate audio latency in milliseconds
        ''' </summary>
        Public ReadOnly Property LatencyMs As Integer
            Get
                Return FMODAudio_GetLatency(_audioHandle)
            End Get
        End Property

        ' IDisposable implementation
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not _disposed Then
                If disposing Then
                    ' Stop audio before cleanup
                    [Stop]()
                End If

                ' Dispose unmanaged resources
                If _audioHandle <> IntPtr.Zero Then
                    DestroyFMODAudio(_audioHandle)
                    _audioHandle = IntPtr.Zero
                End If

                _disposed = True
            End If
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub

        Protected Overrides Sub Finalize()
            Dispose(False)
            MyBase.Finalize()
        End Sub
    End Class

End Namespace