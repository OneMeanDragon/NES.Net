Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' P/Invoke wrapper for native CPU6502 DLL
    ''' </summary>
    Public Class NativeCPU6502
        Implements IDisposable
        Private _disposed As Boolean = False

        ' DLL imports
        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreateCPU() As IntPtr
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyCPU(cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub CPU_Reset(cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub CPU_Clock(cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub CPU_IRQ(cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub CPU_NMI(cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub CPU_ConnectBus(cpu As IntPtr, bus As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CPU_IsComplete(cpu As IntPtr) As Boolean
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CPU_GetPC(cpu As IntPtr) As UShort
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CPU_GetA(cpu As IntPtr) As Byte
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CPU_GetX(cpu As IntPtr) As Byte
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CPU_GetY(cpu As IntPtr) As Byte
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CPU_GetSP(cpu As IntPtr) As Byte
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CPU_GetStatus(cpu As IntPtr) As Byte
        End Function

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CPU_GetClockCount(cpu As IntPtr) As ULong
        End Function

        ' Alias functions for compatibility
        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub ClockCPU(cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub ResetCPU(cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub TriggerNMI(cpu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesPPU, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub TriggerIRQ(cpu As IntPtr)
        End Sub


        Private _cpuHandle As IntPtr

        Public ReadOnly Property NativeHandle As IntPtr
            Get
                Return _cpuHandle
            End Get
        End Property

        Public Sub New()
            _cpuHandle = CreateCPU()
            If _cpuHandle = IntPtr.Zero Then
                Throw New Exception("Failed to create native CPU")
            End If
        End Sub

        Public Sub ConnectBus(bus As IntPtr)
            CPU_ConnectBus(_cpuHandle, bus)
        End Sub

        Public Sub Reset()
            CPU_Reset(_cpuHandle)
        End Sub

        Public Sub Clock()
            CPU_Clock(_cpuHandle)
        End Sub

        Public Sub IRQ()
            CPU_IRQ(_cpuHandle)
        End Sub

        Public Sub NMI()
            CPU_NMI(_cpuHandle)
        End Sub

        Public ReadOnly Property IsComplete As Boolean
            Get
                Return CPU_IsComplete(_cpuHandle)
            End Get
        End Property

        Public Function Complete() As Boolean
            Return CPU_IsComplete(_cpuHandle)
        End Function

        ' Register properties
        Public ReadOnly Property PC As UShort
            Get
                Return CPU_GetPC(_cpuHandle)
            End Get
        End Property

        Public ReadOnly Property A As Byte
            Get
                Return CPU_GetA(_cpuHandle)
            End Get
        End Property

        Public ReadOnly Property X As Byte
            Get
                Return CPU_GetX(_cpuHandle)
            End Get
        End Property

        Public ReadOnly Property Y As Byte
            Get
                Return CPU_GetY(_cpuHandle)
            End Get
        End Property

        Public ReadOnly Property SP As Byte
            Get
                Return CPU_GetSP(_cpuHandle)
            End Get
        End Property

        Public ReadOnly Property Status As Byte
            Get
                Return CPU_GetStatus(_cpuHandle)
            End Get
        End Property

        Public ReadOnly Property ClockCount As ULong
            Get
                Return CPU_GetClockCount(_cpuHandle)
            End Get
        End Property

        ' Debug properties (same as register properties)
        Public ReadOnly Property Debug_PC As UShort
            Get
                Return PC
            End Get
        End Property

        Public ReadOnly Property Debug_SP As Byte
            Get
                Return SP
            End Get
        End Property

        Public ReadOnly Property Debug_A As Byte
            Get
                Return A
            End Get
        End Property

        Public ReadOnly Property Debug_X As Byte
            Get
                Return X
            End Get
        End Property

        Public ReadOnly Property Debug_Y As Byte
            Get
                Return Y
            End Get
        End Property

        Public ReadOnly Property Debug_Status As Byte
            Get
                Return Status
            End Get
        End Property

        Public ReadOnly Property Debug_ClockCount As ULong
            Get
                Return ClockCount
            End Get
        End Property

        ' IDisposable implementation
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not _disposed Then
                If disposing Then
                    ' Dispose managed resources (none currently)
                End If

                ' Dispose unmanaged resources
                If _cpuHandle <> IntPtr.Zero Then
                    DestroyCPU(_cpuHandle)
                    _cpuHandle = IntPtr.Zero
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