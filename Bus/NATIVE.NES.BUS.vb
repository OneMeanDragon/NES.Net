Imports System.Runtime.InteropServices
Imports Nintendo.Core.Audio
Imports Nintendo.Nintendo.NintendoEntertainmentSystem
Imports Nintendo.NintendoEntertainmentSystem.NativeCartridge

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' P/Invoke wrapper for native NES Bus DLL
    ''' </summary>
    Public Class NativeNESBus
        Implements IDisposable

        ' Add a circular buffer for samples
        Private Const AUDIO_RINGBUFFER_SIZE As UInteger = 41983 '8191  ' Power of 2 minus 1
        Private _audioBuffer(AUDIO_RINGBUFFER_SIZE) As Double
        Private _audioBufferWrite As Integer = 0
        Private _audioBufferRead As Integer = 0
        Private _audioBufferLock As New Object()
        Private _lastValidSample As Double = 0.0  ' Hold last valid sample
        Private _bufferUnderrunCount As Long = 0

        Private ReadOnly _audioSystem As New CallbackAudioSystem()
        Public ReadOnly Property AudioSystem As CallbackAudioSystem
            Get
                Return _audioSystem
            End Get
        End Property
        Private Function GetAudioSample(index As Long, time As Double) As Double
            Dim sample As Double
            If Me.PopAudioSample(sample) Then
                Return sample
            Else
                Return 0.0  ' Buffer underrun
            End If
        End Function

        ' Delegates
        Public Delegate Sub DiagnosticCallback(msg As String)

        ' DLL imports
        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateNESBus() As IntPtr
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyNESBus(bus As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_Reset(bus As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_Clock(bus As IntPtr) As Boolean
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_ConnectCartridge(bus As IntPtr, cart As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_ConnectPPU(bus As IntPtr, ppu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_ConnectCPU(bus As IntPtr, cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_ConnectAPU(bus As IntPtr, apu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_CpuRead(bus As IntPtr, addr As UShort, isReadOnly As Boolean) As Byte
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_CpuWrite(bus As IntPtr, addr As UShort, data As Byte)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_SetController(bus As IntPtr, index As Byte, state As Byte)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_GetController(bus As IntPtr, index As Byte) As Byte
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_SetSampleFrequency(bus As IntPtr, sampleRate As UInteger)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_GetAudioSample(bus As IntPtr) As Double
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_GetAudioBufferLevel(bus As IntPtr) As Integer
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_PopAudioSample(bus As IntPtr, ByRef sample As Double) As Boolean
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_GetSystemClockCount(bus As IntPtr) As ULong
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_IsAudioSampleReady(bus As IntPtr) As Boolean
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_SetDiagnosticCallback(bus As IntPtr, callback As DiagnosticCallback)
        End Sub

        ' Instance fields
        Private _busHandle As IntPtr
        Private _diagnosticCallback As DiagnosticCallback
        Private _disposed As Boolean = False

#Region "Temporary Delegates for the CPU and the APU"
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub ClockCPUDelegate()
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub ResetCPUDelegate()
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub TriggerNMIDelegate()
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub TriggerIRQDelegate()

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub UpdateCPUApi(bushandle As IntPtr, api As CPUApi)
        End Sub

        Public Sub CpuClock()
            CPU?.Clock()
        End Sub
        Public Sub CpuReset()
            CPU?.Reset()
        End Sub
        Public Sub NmiTrigger()
            CPU?.NMI()
        End Sub
        Public Sub IrqTrigger()
            CPU?.IRQ()
        End Sub

        Private Structure CPUApi
            Dim ClockCPU As ClockCPUDelegate
            Dim ResetCPU As ResetCPUDelegate
            Dim TriggerNMI As TriggerNMIDelegate
            Dim TriggerIRQ As TriggerIRQDelegate
        End Structure
        Private _cpuApi As CPUApi

        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub ClockAPUDelegate()
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub ResetAPUDelegate()
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Function APU_CpuReadDelegate(addr As UInt16) As Byte
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub APU_CpuWriteDelegate(addr As UInt16, data As Byte)
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Function APU_GetOutputSampleDelegate() As Double

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub UpdateAPUApi(bushandle As IntPtr, api As APUApi)
        End Sub

        Public Sub ApuClock()
            APU?.Clock()
        End Sub
        Public Sub ApuReset()
            APU?.Reset()
        End Sub
        Public Function ApuCpuRead(addr As UInt16) As Byte
            Return APU?.CpuRead(addr)
        End Function
        Public Sub ApuCpuWrite(addr As UInt16, data As Byte)
            APU?.CpuWrite(addr, data)
        End Sub
        Public Function ApuGetOutputSample() As Double
            Return APU?.GetOutputSample()
        End Function

        Private Structure APUApi
            Dim ClockAPU As ClockAPUDelegate
            Dim ResetAPU As ResetAPUDelegate
            Dim APU_CpuRead As APU_CpuReadDelegate
            Dim APU_CpuWrite As APU_CpuWriteDelegate
            Dim APU_GetOutputSample As APU_GetOutputSampleDelegate
        End Structure
        Private _apuApi As APUApi

#End Region
        Public ReadOnly CPU As New CPU6502()
        Public PPU As NativePPU2C02 'New NetPPU2C02() ' NativePPU2C02
        Public ReadOnly APU As New em2A03()

        Private Const AUDIO_SAMPLE_RATE As UInt32 = 44100

        Public Sub New()
            _cpuApi.ClockCPU = New ClockCPUDelegate(AddressOf CpuClock)
            _cpuApi.ResetCPU = New ResetCPUDelegate(AddressOf CpuReset)
            _cpuApi.TriggerNMI = New TriggerNMIDelegate(AddressOf NmiTrigger)
            _cpuApi.TriggerIRQ = New TriggerIRQDelegate(AddressOf IrqTrigger)

            _apuApi.ClockAPU = New ClockAPUDelegate(AddressOf ApuClock)
            _apuApi.ResetAPU = New ResetAPUDelegate(AddressOf ApuReset)
            _apuApi.APU_CpuRead = New APU_CpuReadDelegate(AddressOf ApuCpuRead)
            _apuApi.APU_CpuWrite = New APU_CpuWriteDelegate(AddressOf ApuCpuWrite)
            _apuApi.APU_GetOutputSample = New APU_GetOutputSampleDelegate(AddressOf ApuGetOutputSample)

            _busHandle = CreateNESBus()
            If _busHandle = IntPtr.Zero Then
                Throw New Exception("Failed to create native NES Bus")
            End If

            UpdateCPUApi(_busHandle, _cpuApi)
            UpdateAPUApi(_busHandle, _apuApi)

            ' Connect CPU to this bus
            CPU.ConnectBus(Me)


            SetSampleFrequency(AUDIO_SAMPLE_RATE)

            If Not _audioSystem.Initialize(44100, 1, 16, 512, AddressOf GetAudioSample) Then
                Debug.WriteLine("[Bus] Warning: Audio system failed to initialize")
            End If
            'For i = 0 To 2047
            '    _audioBuffer(i) = 0.0
            'Next
            '_audioBufferWrite = 2048
        End Sub

        Public Sub Reset()
            Bus_Reset(_busHandle)
        End Sub

        Public Function Clock() As Boolean
            Return Bus_Clock(_busHandle)
        End Function

        Public Sub ConnectCartridge(cartHandle As IntPtr)
            If Not (PPU Is Nothing) Then
                PPU.Dispose()
            End If
            PPU = New NativePPU2C02(cartHandle)
            ConnectPPU(PPU.NativeHandle)
            Bus_ConnectCartridge(_busHandle, cartHandle)
        End Sub

        Public Sub ConnectPPU(ppuHandle As IntPtr)
            Bus_ConnectPPU(_busHandle, ppuHandle)
        End Sub

        Public Sub ConnectCPU(cpuHandle As IntPtr)
            Bus_ConnectCPU(_busHandle, cpuHandle)
        End Sub

        Public Sub ConnectAPU(apuHandle As IntPtr)
            Bus_ConnectAPU(_busHandle, apuHandle)
        End Sub

        Public Function CpuRead(addr As UShort, Optional isReadOnly As Boolean = False) As Byte
            Return Bus_CpuRead(_busHandle, addr, isReadOnly)
        End Function

        Public Sub CpuWrite(addr As UShort, data As Byte)
            Bus_CpuWrite(_busHandle, addr, data)
        End Sub

        Public Property Controller(index As Integer) As Byte
            Get
                Return Bus_GetController(_busHandle, CByte(index))
            End Get
            Set(value As Byte)
                Bus_SetController(_busHandle, CByte(index), value)
            End Set
        End Property

        Public Sub SetSampleFrequency(sampleRate As UInteger)
            Bus_SetSampleFrequency(_busHandle, sampleRate)
        End Sub

        Public ReadOnly Property AudioSample As Double
            Get
                Return Bus_GetAudioSample(_busHandle)
            End Get
        End Property

        Public ReadOnly Property AudioBufferLevel As Integer
            Get
                Return Bus_GetAudioBufferLevel(_busHandle)
            End Get
        End Property

        Public Function PopAudioSample(ByRef sample As Double) As Boolean
            Return Bus_PopAudioSample(_busHandle, sample)
        End Function

        Public ReadOnly Property SystemClockCount As ULong
            Get
                Return Bus_GetSystemClockCount(_busHandle)
            End Get
        End Property

        Public ReadOnly Property AudioSampleReady As Boolean
            Get
                Return Bus_IsAudioSampleReady(_busHandle)
            End Get
        End Property

        Public Sub SetDiagnosticCallback(callback As Action(Of String))
            _diagnosticCallback = Sub(msg As String) callback(msg)
            Bus_SetDiagnosticCallback(_busHandle, _diagnosticCallback)
        End Sub

        ' IDisposable implementation
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not _disposed Then
                If disposing Then
                    ' Dispose managed resources (none for Bus currently)
                End If

                ' Dispose unmanaged resources
                If _busHandle <> IntPtr.Zero Then
                    DestroyNESBus(_busHandle)
                    _busHandle = IntPtr.Zero
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