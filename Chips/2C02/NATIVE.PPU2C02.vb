Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' P/Invoke wrapper for native PPU2C02 DLL
    ''' </summary>
    Public Class NativePPU2C02x

        ' Delegates for callbacks
        Public Delegate Sub PixelCallback(x As Integer, y As Integer, r As Byte, g As Byte, b As Byte)
        Public Delegate Sub DiagnosticCallback(msg As String)

        ' DLL imports
        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreatePPU(cart As IntPtr) As IntPtr
        End Function

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyPPU(ppu As IntPtr)
        End Sub

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_Reset(ppu As IntPtr)
        End Sub

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_Clock(ppu As IntPtr)
        End Sub

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function PPU_CpuRead(ppu As IntPtr, addr As UShort, rdOnly As Boolean) As Byte
        End Function

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_CpuWrite(ppu As IntPtr, addr As UShort, data As Byte)
        End Sub

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function PPU_IsFrameComplete(ppu As IntPtr) As Boolean
        End Function

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_SetFrameComplete(ppu As IntPtr, value As Boolean)
        End Sub

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function PPU_GetNmiRequested(ppu As IntPtr) As Boolean
        End Function

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_ClearNmiRequested(ppu As IntPtr)
        End Sub

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_SetPixelCallback(ppu As IntPtr, callback As PixelCallback)
        End Sub

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_SetDiagnosticCallback(ppu As IntPtr, callback As DiagnosticCallback)
        End Sub

        <DllImport("NesEmulatorCore.dll", CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_GetPatternTable(ppu As IntPtr, table As Byte, palette As Byte, buffer As Byte())
        End Sub

        ' Instance fields
        Private _ppuHandle As IntPtr
        Private _pixelCallback As PixelCallback
        Private _diagnosticCallback As DiagnosticCallback
        Private _screen As GraphicsObjects.Sprite

        Public Sub New(cartridge As IntPtr)
            _ppuHandle = CreatePPU(cartridge)
            If _ppuHandle = IntPtr.Zero Then
                Throw New Exception("Failed to create native PPU")
            End If

            _screen = New GraphicsObjects.Sprite(256, 240)

            ' Create callback that draws to our Sprite
            _pixelCallback = Sub(x As Integer, y As Integer, r As Byte, g As Byte, b As Byte)
                                 _screen.SetPixel(x, y, New GraphicsObjects.Pixel(r, g, b))
                             End Sub

            PPU_SetPixelCallback(_ppuHandle, _pixelCallback)
        End Sub

        Public ReadOnly Property Screen As GraphicsObjects.Sprite
            Get
                Return _screen
            End Get
        End Property

        Public Sub Reset()
            PPU_Reset(_ppuHandle)
        End Sub

        Public Sub Clock()
            PPU_Clock(_ppuHandle)
        End Sub

        Public Function CpuRead(addr As UShort, Optional rdOnly As Boolean = False) As Byte
            Return PPU_CpuRead(_ppuHandle, addr, rdOnly)
        End Function

        Public Sub CpuWrite(addr As UShort, data As Byte)
            PPU_CpuWrite(_ppuHandle, addr, data)
        End Sub

        Public Property FrameComplete As Boolean
            Get
                Return PPU_IsFrameComplete(_ppuHandle)
            End Get
            Set(value As Boolean)
                PPU_SetFrameComplete(_ppuHandle, value)
            End Set
        End Property

        Public Property NmiRequested As Boolean
            Get
                Return PPU_GetNmiRequested(_ppuHandle)
            End Get
            Set(value As Boolean)
                If Not value Then PPU_ClearNmiRequested(_ppuHandle)
            End Set
        End Property

        Public Function GetPatternTable(table As Byte, palette As Byte) As GraphicsObjects.Sprite
            Dim buffer(128 * 128 * 4 - 1) As Byte
            PPU_GetPatternTable(_ppuHandle, table, palette, buffer)

            Dim sprite As New GraphicsObjects.Sprite(128, 128)
            For y = 0 To 127
                For x = 0 To 127
                    Dim idx = (y * 128 + x) * 4
                    sprite.SetPixel(x, y, New GraphicsObjects.Pixel(buffer(idx), buffer(idx + 1), buffer(idx + 2)))
                Next
            Next

            Return sprite
        End Function

        Public Sub SetDiagnosticCallback(callback As Action(Of String))
            _diagnosticCallback = Sub(msg As String) callback(msg)
            PPU_SetDiagnosticCallback(_ppuHandle, _diagnosticCallback)
        End Sub

        Protected Overrides Sub Finalize()
            If _ppuHandle <> IntPtr.Zero Then
                DestroyPPU(_ppuHandle)
                _ppuHandle = IntPtr.Zero
            End If
            MyBase.Finalize()
        End Sub
    End Class

End Namespace