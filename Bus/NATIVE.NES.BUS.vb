Imports System.Runtime.InteropServices
Imports Nintendo.NintendoEntertainmentSystem.NativeCartridge

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' P/Invoke wrapper for native NES Bus DLL
    ''' </summary>
    Public Class NativeNESBus
        Implements IDisposable

#Region "NativeNESBus Delegate Definitions"
        ''' <summary>
        ''' Sets the callback within the dll to point at the provided delegate
        ''' </summary>
        ''' <param name="bus"></param>
        ''' <param name="callback"></param>
        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub BusSetDiagnosticLogCallback(bus As IntPtr, callback As DLLPath.DiagnosticLogDelegate)
        End Sub

        ''' <summary>
        ''' Enables the logging within the dll
        ''' </summary>
        ''' <param name="bus"></param>
        ''' <param name="enable"></param>
        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub BusEnableDiagnosticLogger(bus As IntPtr, enable As Boolean)
        End Sub

        ''' <summary>
        ''' The Logging delegate our dll will be firing its diagnostics messages to. 
        ''' </summary>
        ''' <param name="message"></param>
        Private Sub BusDiagnosticLogger(ByVal message As String)
            Console.WriteLine("Bus: " & message)
        End Sub
#End Region

        Private _audio As FMODAudioNative
        Public Property AudioSystem As FMODAudioNative
            Get
                Return _audio
            End Get
            Set(value As FMODAudioNative)
                _audio = value
            End Set
        End Property


        ' Bus functions
        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_CpuRead(bus As IntPtr, addr As UShort, isReadOnly As Boolean) As Byte
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_CpuWrite(bus As IntPtr, addr As UShort, data As Byte)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_SetController(bus As IntPtr, index As Byte, state As Byte)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_GetController(bus As IntPtr, index As Byte) As Byte
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateNESBus() As IntPtr
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyNESBus(bus As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_Tick(bus As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_Reset(bus As IntPtr, coldstart As Boolean)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_Stop(bus As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_ConnectCartridge(bus As IntPtr, cart As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_ConnectPPU(bus As IntPtr, ppu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_ConnectCPU(bus As IntPtr, cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_ConnectAPU(bus As IntPtr, apu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function Bus_GetAudioSystem(bus As IntPtr) As IntPtr
        End Function

        ' FMODAudioSystem functions
        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function FMODAudio_Initialize(audio As IntPtr, sampleRate As Integer, bufferSize As Integer) As Boolean
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub FMODAudio_Start(audio As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub FMODAudio_Stop(audio As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub FMODAudio_SetVolume(audio As IntPtr, volume As Single)
        End Sub

        ' Component creation
        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreatePPU() As IntPtr
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateCPU() As IntPtr
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateAPU() As IntPtr
        End Function

        '==============
        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub Bus_PreFillAudioBuffer(bus As IntPtr, numSamples As Integer)
        End Sub
        '==============

        ' Instance fields
        Private _busHandle As IntPtr
        Private _diagnosticCallback As DLLPath.DiagnosticLogDelegate
        Private _disposed As Boolean = False

        Public ReadOnly CPU As New NativeCPU6502()
        Public ReadOnly PPU As New NativePPU2C02()
        Public ReadOnly APU As New NativeAPU2A03()

        Public Sub New()
            _diagnosticCallback = New DLLPath.DiagnosticLogDelegate(AddressOf BusDiagnosticLogger)

            _busHandle = CreateNESBus()
            If _busHandle = IntPtr.Zero Then
                Throw New Exception("Failed to create native NES Bus")
            End If
            BusSetDiagnosticLogCallback(_busHandle, _diagnosticCallback)
            BusEnableDiagnosticLogger(_busHandle, True)

            ConnectCPU(CPU.NativeHandle)
            ConnectPPU(PPU.NativeHandle)
            ConnectAPU(APU.NativeHandle)

            AudioSystem = New FMODAudioNative(Bus_GetAudioSystem(_busHandle))
            ' Initialize audio - use 2048 for very stable playback 44100
            If FMODAudio_Initialize(AudioSystem.NativeHandle, 44100, 512) Then
                Console.WriteLine("Audio initialized with 2048 sample buffer")
            Else
                Console.WriteLine("Failed to initialize audio")
            End If
        End Sub
        ' Also add a method to check buffer status:
        Public Function GetAudioBufferStatus() As String
            ' The debug output happens automatically in Tick()
            ' Check your console output every second
            Return "Check console for buffer status"
        End Function

        ''' <summary>
        ''' coldstart true means turn the power on.
        ''' coldstart false means warm reset.
        ''' Hitting Reset on the bus will reset all the chips internally and passes along the coldstart bool.
        ''' </summary>
        Public Sub Reset(coldstart As Boolean)
            Bus_Reset(_busHandle, coldstart)
        End Sub

        Public Sub [Stop]()
            Bus_Stop(_busHandle)
        End Sub

        'Public Function Clock() As Boolean
        '    Return Bus_Clock(_busHandle)
        'End Function

        Public Sub Tick()
            Bus_Tick(_busHandle)
        End Sub

        Public Sub ConnectCartridge(cartHandle As IntPtr)
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