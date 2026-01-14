Imports System.Runtime.InteropServices

Namespace TestingGrounds

    Public Class TestCartridge
        Implements IDisposable

        Private Const DllPath As String = "NesCartridge.dll"

        <UnmanagedFunctionPointer(CallingConvention.StdCall)>
        Public Delegate Sub DiagnosticDelegate(ByVal message As String)

        <DllImport(DllPath, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateCartridge() As IntPtr
        End Function

        <DllImport(DllPath, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyCartridge(ByVal cart As IntPtr)
        End Sub

        <DllImport(DllPath, CallingConvention:=CallingConvention.Cdecl, CharSet:=CharSet.Ansi)>
        Private Shared Function LoadRom(cart As IntPtr, path As String, callback As DiagnosticDelegate) As Boolean
        End Function

        Private _nativePtr As IntPtr
        Private _disposedValue As Boolean = False ' To detect redundant calls

        Private _diagCallback As DiagnosticDelegate
        Private disposedValue As Boolean

        Public Sub New(filePath As String)
            _nativePtr = CreateCartridge()
            If _nativePtr = IntPtr.Zero Then
                Throw New Exception("Failed to allocate native Cartridge.")
            End If

            ' 3. Instantiate the delegate with your VB function
            _diagCallback = New DiagnosticDelegate(AddressOf MyDiagnosticLogger)

            ' 4. Pass the delegate into the DLL function
            Dim result = LoadRom(_nativePtr, filePath, _diagCallback)
            If Not result Then
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
                End If

                _disposedValue = True
            End If
        End Sub

        ' Finalizer: The "safety net" in case Dispose() was never called
        Protected Overrides Sub Finalize()
            Dispose(False)
            MyBase.Finalize()
        End Sub

#Region "Diagnostics Logs"
        Private Sub MyDiagnosticLogger(ByVal message As String)
            Debug.WriteLine("C++ Diagnostic: " & message)
        End Sub
#End Region

    End Class



End Namespace