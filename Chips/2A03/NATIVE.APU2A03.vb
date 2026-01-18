Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' P/Invoke wrapper for native APU2A03 DLL
    ''' </summary>
    Public Class NativeAPU2A03
        Implements IDisposable

        ' DLL imports
        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateAPU() As IntPtr
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyAPU(apu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub APU_CpuWrite(apu As IntPtr, addr As UShort, data As Byte)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function APU_CpuRead(apu As IntPtr, addr As UShort) As Byte
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub APU_Clock(apu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function APU_GetOutputSample(apu As IntPtr) As Double
        End Function

        ' Instance fields
        Private _apuHandle As IntPtr
        Public ReadOnly Property NativeHandle As IntPtr
            Get
                Return _apuHandle
            End Get
        End Property

        Private _disposed As Boolean = False

        Public Sub New()
            _apuHandle = CreateAPU()
            If _apuHandle = IntPtr.Zero Then
                Throw New Exception("Failed to create native APU")
            End If
        End Sub

        Public ReadOnly Property Handle As IntPtr
            Get
                Return _apuHandle
            End Get
        End Property

        Public Sub CpuWrite(addr As UShort, data As Byte)
            APU_CpuWrite(_apuHandle, addr, data)
        End Sub

        Public Function CpuRead(addr As UShort) As Byte
            Return APU_CpuRead(_apuHandle, addr)
        End Function

        Public Sub Clock()
            APU_Clock(_apuHandle)
        End Sub

        Public Function GetOutputSample() As Double
            Return APU_GetOutputSample(_apuHandle)
        End Function

        ' IDisposable implementation
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not _disposed Then
                If disposing Then
                    ' Dispose managed resources (none for APU)
                End If

                ' Dispose unmanaged resources
                If _apuHandle <> IntPtr.Zero Then
                    DestroyAPU(_apuHandle)
                    _apuHandle = IntPtr.Zero
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