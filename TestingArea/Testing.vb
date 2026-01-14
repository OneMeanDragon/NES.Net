Imports System.Runtime.InteropServices

#Const DIAGNOSE_CREATE_CARTRIDGE_CLASS = True

Namespace TestingGrounds

    Public Class TestCartridge
        Implements IDisposable

#Region "DLL Imports"
        Private Const DllPath As String = "NesCartridge.dll"

#Region "Cartridge Delegate Definitions"
        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub DiagnosticLogDelegate(ByVal message As String)

        <DllImport(DllPath, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub CartridgeSetDiagnosticLogCallback(cart As IntPtr, callback As DiagnosticLogDelegate)
        End Sub
#End Region

        <DllImport(DllPath, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateCartridge() As IntPtr
        End Function
        <DllImport(DllPath, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateCartridgeDiag(callback As DiagnosticLogDelegate) As IntPtr
        End Function

        <DllImport(DllPath, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyCartridge(ByVal cart As IntPtr)
        End Sub

        <DllImport(DllPath, CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)>
        Private Shared Function LoadCartridge(cart As IntPtr, path As String) As Boolean
        End Function
#End Region

        Private _nativePtr As IntPtr
        Private _disposedValue As Boolean = False ' To detect redundant calls

        Private _diagCallback As DiagnosticLogDelegate
        Private disposedValue As Boolean

        Public Sub New(filePath As String)
            _diagCallback = New DiagnosticLogDelegate(AddressOf DiagnosticLogger)

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

            If Not LoadCartridge(_nativePtr, filePath) Then
                Throw New Exception("Failed to load ROM")
            End If
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

#Region "Diagnostics"
        Private Sub DiagnosticLogger(ByVal message As String)
            Debug.WriteLine("C++ Diagnostic: " & message)
        End Sub
#End Region

    End Class



End Namespace