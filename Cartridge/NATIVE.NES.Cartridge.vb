Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices


#Const DIAGNOSE_CREATE_CARTRIDGE_CLASS = True

Namespace NintendoEntertainmentSystem
    Public Module CartridgeGlobal
        Public Cart As NativeCartridge
    End Module

    Public Module DLLPath
        Public Const NesCartridge As String = "NesCartridge.dll"
        Public Const NesChipset As String = "NesChipset.dll"
        'Diagnostics
        <UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet:=CharSet.Ansi)>
        Public Delegate Sub DiagnosticLogDelegate(ByVal message As String)
    End Module

    Public Enum [MirrorMode] As Byte
        Hardware = 0        ' Determined by cart hardware
        Horizontal = 1      ' Vertical arrangement
        Vertical = 2        ' Horizontal arrangement  
        OneScreenLo = 3     ' Single screen, lower bank
        OneScreenHi = 4     ' Single screen, upper bank
        FourScreen = 5      ' Four-screen (extra VRAM)
    End Enum

    Public Class NativeMapperBase
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CartridgeMapper(cart As IntPtr) As IntPtr
            ' Returned Pointer is a unique_ptr<MapperBase>().get()
            ' we will need this pointer for our exported mapper functions
        End Function
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function MapperIsIrqActive(mapper As IntPtr) As Boolean
        End Function
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub MapperClearIrq(mapper As IntPtr)
        End Sub
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function MapperGetMirrorMode(mapper As IntPtr) As MirrorMode
        End Function

        Private _nativePtr As IntPtr = IntPtr.Zero ' Pointer to our cartridge class do not destroy it here

        Public Sub New(_cartPtr As IntPtr)
            _nativePtr = _cartPtr
        End Sub

        Public Function IsIrqActive() As Boolean
            Dim _mapperPtr As IntPtr = CartridgeMapper(_nativePtr)
            Return MapperIsIrqActive(_mapperPtr)
        End Function

        Public Sub ClearIrq()
            Dim _mapperPtr As IntPtr = CartridgeMapper(_nativePtr)
            MapperClearIrq(_mapperPtr)
        End Sub

        Public Sub ScanlineCounter()
            ' dosent do anything anyways
        End Sub

        Public Function GetMirrorMode() As MirrorMode
            Dim _mapperPtr As IntPtr = CartridgeMapper(_nativePtr)
            Return MapperGetMirrorMode(_mapperPtr)
        End Function

    End Class

    Public Class NativeCartridge
        Implements IDisposable

#Region "DLL Imports"

#Region "Cartridge Delegate Definitions"
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub CartridgeSetDiagnosticLogCallback(cart As IntPtr, callback As DLLPath.DiagnosticLogDelegate)
        End Sub
#End Region

        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateCartridge() As IntPtr
        End Function
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateCartridgeDiag(callback As DLLPath.DiagnosticLogDelegate) As IntPtr
        End Function

        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyCartridge(ByVal cart As IntPtr)
        End Sub

        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)>
        Private Shared Function LoadCartridge(cart As IntPtr, path As String) As Boolean
        End Function

        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CartCpuRead(ByVal cart As IntPtr, ByVal addr As UInt16, ByRef data As Byte) As Boolean
        End Function

        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CartCpuWrite(ByVal cart As IntPtr, ByVal addr As UInt16, ByVal data As Byte) As Boolean
        End Function

        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CartPpuRead(ByVal cart As IntPtr, ByVal addr As UInt16, ByRef data As Byte) As Boolean
        End Function

        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CartPpuWrite(ByVal cart As IntPtr, ByVal addr As UInt16, ByVal data As Byte) As Boolean
        End Function
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CartridgeGetMirrorMode(ByVal cart As IntPtr) As MirrorMode
        End Function
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CartridgeIsLoaded(ByVal cart As IntPtr) As Boolean
        End Function
        <DllImport(DLLPath.NesCartridge, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub CartridgeEnableLogging(ByVal cart As IntPtr, ByVal enable As Boolean)
        End Sub

#End Region

        Private _nativePtr As IntPtr = IntPtr.Zero
        Public ReadOnly Property NativeHandle As IntPtr
            Get
                Return _nativePtr
            End Get
        End Property

        Private _disposedValue As Boolean = False ' To detect redundant calls

        Private _diagCallback As DLLPath.DiagnosticLogDelegate
        Private disposedValue As Boolean

        Private _mapper As NativeMapperBase

        Public ReadOnly Property Mapper As NativeMapperBase
            Get
                Return _mapper
            End Get
        End Property

        Public Sub New(ByVal filePath As String)
            _diagCallback = New DLLPath.DiagnosticLogDelegate(AddressOf DiagnosticLogger)

#If DIAGNOSE_CREATE_CARTRIDGE_CLASS Then
            _nativePtr = CreateCartridgeDiag(_diagCallback)
            If _nativePtr = IntPtr.Zero Then
                Throw New Exception("Failed to allocate native Cartridge.")
            End If
#Else
            ' If we creating the diagnostic manually
            _nativePtr = CreateCartridge()
            If _nativePtr = IntPtr.Zero Then
                Throw New Exception("Failed to allocate native Cartridge.")
            End If
            DiagnosticLogger("Native Cartridge instance created.")
            CartridgeSetDiagnosticLogCallback(_nativePtr, _diagCallback)
#End If

            'Turn on or off the Logger callback
            CartridgeEnableLogging(_nativePtr, False)

            If Not LoadCartridge(_nativePtr, filePath) Then
                Throw New Exception("Failed to load ROM")
            End If
            _mapper = New NativeMapperBase(_nativePtr)
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            ' Dispose of unmanaged resources
            Dispose(True)
            ' Tell the GC not to call the finalizer because we already cleaned up
            GC.SuppressFinalize(Me)
        End Sub

        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not _disposedValue Then
                ' If disposing is True, we are being called by user code
                ' If False, we are being called by the Finalizer (GC)
                If _nativePtr <> IntPtr.Zero Then
                    ' Call the C++ DLL to delete the object
                    DestroyCartridge(_nativePtr)
                    _nativePtr = IntPtr.Zero
                    DiagnosticLogger("Cartridge: Disposed.")
                End If
                _disposedValue = True
            End If
        End Sub

        ' Finalizer: The "safety net" in case Dispose() was never called
        Protected Overrides Sub Finalize()
            Dispose(False)
            MyBase.Finalize()
        End Sub

#Region "Functionality"

        Public Function CpuRead(ByVal addr As UInt16, ByRef data As Byte) As Boolean
            Return CartCpuRead(_nativePtr, addr, data)
        End Function

        Public Function CpuWrite(ByVal addr As UInt16, ByVal data As Byte) As Boolean
            Return CartCpuWrite(_nativePtr, addr, data)
        End Function

        Public Function PpuRead(ByVal addr As UInt16, ByRef data As Byte) As Boolean
            Return CartPpuRead(_nativePtr, addr, data)
        End Function

        Public Function PpuWrite(ByVal addr As UInt16, ByVal data As Byte) As Boolean
            Return CartPpuWrite(_nativePtr, addr, data)
        End Function

        Public ReadOnly Property MirrorMode As MirrorMode
            Get
                Return CartridgeGetMirrorMode(_nativePtr)
            End Get
        End Property

        Public ReadOnly Property GetMapper() As NativeMapperBase
            Get
                Return _mapper
            End Get
        End Property

        Public Sub LoadFromFile(ByVal path As String)
            LoadCartridge(_nativePtr, path)
        End Sub

        Public ReadOnly Property IsLoaded As Boolean
            Get
                Return CartridgeIsLoaded(_nativePtr)
            End Get
        End Property
#End Region





#Region "Diagnostics"
        Private Sub DiagnosticLogger(ByVal message As String)
            Console.WriteLine("Debug: " & message)
        End Sub
#End Region

    End Class



End Namespace
