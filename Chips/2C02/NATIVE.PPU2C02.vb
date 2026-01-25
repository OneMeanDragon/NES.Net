Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' P/Invoke wrapper for native PPU2C02 DLL
    ''' </summary>
    Public Class NativePPU2C02
        Implements IDisposable
        Private _disposed As Boolean = False

        ' Delegates for callbacks
        Public Delegate Sub PixelCallback(x As Integer, y As Integer, r As Byte, g As Byte, b As Byte)
        Public Delegate Sub DiagnosticCallback(msg As String)

        ' DLL imports
        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function CreatePPU() As IntPtr
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub DestroyPPU(ppu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_Clock(ppu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function PPU_CpuRead(ppu As IntPtr, addr As UShort, rdOnly As Boolean) As Byte
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_CpuWrite(ppu As IntPtr, addr As UShort, data As Byte)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function PPU_IsFrameComplete(ppu As IntPtr) As Boolean
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_SetFrameComplete(ppu As IntPtr, value As Boolean)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function PPU_GetNmiRequested(ppu As IntPtr) As Boolean
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_ClearNmiRequested(ppu As IntPtr)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_SetPixelCallback(ppu As IntPtr, callback As PixelCallback)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_SetDiagnosticCallback(ppu As IntPtr, callback As DiagnosticCallback)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_GetPatternTable(ppu As IntPtr, table As Byte, palette As Byte, buffer As Byte())
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Function PPU_GetOAMByte(ppu As IntPtr, oamAddr As Byte) As Byte
        End Function

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_SetOAMByte(ppu As IntPtr, oamAddr As Byte, data As Byte)
        End Sub

        <DllImport(DLLPath.NesChipset, CallingConvention:=CallingConvention.Cdecl)>
        Private Shared Sub PPU_GetColorFromPalette(ppu As IntPtr, palette As Byte, pixel As Byte, ByRef r As Byte, ByRef g As Byte, ByRef b As Byte)
        End Sub


        Private _ppuHandle As IntPtr
        Public ReadOnly Property NativeHandle As IntPtr
            Get
                Return _ppuHandle
            End Get
        End Property


        ' Instance fields
        Private _pixelCallback As PixelCallback
        Private _diagnosticCallback As DiagnosticCallback
        Private _screen As GraphicsObjects.Sprite
        Private _oamWrapper As OAMWrapper

        ' OAM wrapper class
        Public Class OAMWrapper
            Private _ppuHandle As IntPtr

            Public Sub New(ppuHandle As IntPtr)
                _ppuHandle = ppuHandle
            End Sub

            ' Indexer to access OAM entries
            Default Public ReadOnly Property Item(index As Byte) As OAMEntryWrapper
                Get
                    Return New OAMEntryWrapper(_ppuHandle, index)
                End Get
            End Property
        End Class

        ' Individual OAM entry wrapper
        Public Class OAMEntryWrapper
            Private _ppuHandle As IntPtr
            Private _index As Byte

            Public Sub New(ppuHandle As IntPtr, index As Byte)
                _ppuHandle = ppuHandle
                _index = index
            End Sub

            Public Sub SetByteAt(byteIndex As Byte, value As Byte)
                Dim oamAddr As Byte = _index 'CByte(_index * 4 + (byteIndex And &H3))
                PPU_SetOAMByte(_ppuHandle, oamAddr, value)
            End Sub

            Public Function GetByteAt(byteIndex As Byte) As Byte
                Dim oamAddr As Byte = _index 'CByte(_index * 4 + (byteIndex And &H3))
                Return PPU_GetOAMByte(_ppuHandle, oamAddr)
            End Function

            Public Property Y As Byte
                Get
                    Return GetByteAt(0)
                End Get
                Set(value As Byte)
                    SetByteAt(0, value)
                End Set
            End Property

            Public Property TileID As Byte
                Get
                    Return GetByteAt(1)
                End Get
                Set(value As Byte)
                    SetByteAt(1, value)
                End Set
            End Property

            Public Property Attributes As Byte
                Get
                    Return GetByteAt(2)
                End Get
                Set(value As Byte)
                    SetByteAt(2, value)
                End Set
            End Property

            Public Property X As Byte
                Get
                    Return GetByteAt(3)
                End Get
                Set(value As Byte)
                    SetByteAt(3, value)
                End Set
            End Property
        End Class

        Public Sub Log(msg As String)
            Console.WriteLine("PPU: " & msg)
        End Sub

        Public Sub New()
            _ppuHandle = CreatePPU()

            If _ppuHandle = IntPtr.Zero Then
                Throw New Exception("Failed to create native PPU")
            End If

            SetDiagnosticCallback(AddressOf Log)


            _screen = New GraphicsObjects.Sprite(256, 240)
            _oamWrapper = New OAMWrapper(_ppuHandle)

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

        Public ReadOnly Property OAM As OAMWrapper
            Get
                Return _oamWrapper
            End Get
        End Property

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

        Public Function GetColorFromPalette(palette As Byte, pixel As Byte) As GraphicsObjects.Pixel
            Dim r, g, b As Byte
            PPU_GetColorFromPalette(_ppuHandle, palette, pixel, r, g, b)
            Return New GraphicsObjects.Pixel(r, g, b)
        End Function

        Public Sub SetDiagnosticCallback(callback As Action(Of String))
            _diagnosticCallback = Sub(msg As String) callback(msg)
            PPU_SetDiagnosticCallback(_ppuHandle, _diagnosticCallback)
        End Sub

        ' IDisposable implementation
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not _disposed Then
                If disposing Then
                    ' Dispose managed resources
                    _screen?.Dispose()
                End If

                ' Dispose unmanaged resources
                If _ppuHandle <> IntPtr.Zero Then
                    DestroyPPU(_ppuHandle)
                    _ppuHandle = IntPtr.Zero
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