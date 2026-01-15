Imports System.Drawing
Imports System.Drawing.Imaging
Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports Nintendo.NintendoEntertainmentSystem

Namespace Core

    ''' <summary>
    ''' High-performance renderer for NES emulator output
    ''' Handles all drawing operations with optimized memory access
    ''' </summary>
    Public NotInheritable Class Renderer
        Implements IDisposable

#Region "Constants"
        ' Layout constants
        Private Const GAME_WIDTH As Integer = 256
        Private Const GAME_HEIGHT As Integer = 240
        Private Const MARGIN As Integer = 2
        Private Const PATTERN_SIZE As Integer = 128
        Private Const PATTERN_MARGIN As Integer = 2
        Private Const SWATCH_SIZE As Integer = 6

        ' Total canvas size
        Public Const CANVAS_WIDTH As Integer = GAME_WIDTH + 4 + ((PATTERN_SIZE + 2) * 2)
        Public Const CANVAS_HEIGHT As Integer = GAME_HEIGHT + 4
        Public ReadOnly Property CanvasSize As Size = New Size(CANVAS_WIDTH, CANVAS_HEIGHT)
#End Region

#Region "State"
        Private _backBuffer As Bitmap
        Private _isDisposed As Boolean
        Private _selectedPalette As Integer = 0
        Private _prevSelectedPalette As Integer = 0

        ' Dirty flags for selective rendering
        Private _needsPatternRedraw As Boolean = True
        Private _needsPaletteRedraw As Boolean = True
#End Region

#Region "Properties"
        ''' <summary>
        ''' Current selected palette index (0-7)
        ''' </summary>
        Public Property SelectedPalette As Integer
            Get
                Return _selectedPalette
            End Get
            Set(value As Integer)
                If value <> _selectedPalette Then
                    _prevSelectedPalette = _selectedPalette
                    _selectedPalette = value
                    _needsPatternRedraw = True
                    _needsPaletteRedraw = True
                End If
            End Set
        End Property

        ''' <summary>
        ''' Gets the current back buffer (read-only access)
        ''' </summary>
        Public ReadOnly Property BackBuffer As Bitmap
            Get
                Return _backBuffer
            End Get
        End Property
#End Region

#Region "Constructor & Disposal"
        Public Sub New()
            _backBuffer = New Bitmap(CANVAS_WIDTH, CANVAS_HEIGHT, PixelFormat.Format32bppPArgb)
            Clear(GraphicsObjects.PixelColors.DarkGrey)
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                _backBuffer?.Dispose()
                _backBuffer = Nothing
                _isDisposed = True
            End If
        End Sub
#End Region

#Region "Core Drawing Methods"
        ''' <summary>
        ''' Clear the entire canvas to a solid color
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub Clear(color As GraphicsObjects.Pixel)
            Using g As Graphics = Graphics.FromImage(_backBuffer)
                g.Clear(color.ToColor())
            End Using
        End Sub

        ''' <summary>
        ''' Draw a sprite at the specified position with optional clipping
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub DrawSprite(x As Integer, y As Integer, sprite As GraphicsObjects.Sprite)
            If sprite Is Nothing Then Return

            ' Early clipping check
            Dim rectCanvas As New Rectangle(0, 0, _backBuffer.Width, _backBuffer.Height)
            Dim rectSprite As New Rectangle(x, y, sprite.Width, sprite.Height)
            If Not rectCanvas.IntersectsWith(rectSprite) Then Return

            ' Calculate effective clipping
            Dim dstX As Integer = Math.Max(0, x)
            Dim dstY As Integer = Math.Max(0, y)
            Dim srcX As Integer = If(x < 0, -x, 0)
            Dim srcY As Integer = If(y < 0, -y, 0)
            Dim copyW As Integer = Math.Min(_backBuffer.Width - dstX, sprite.Width - srcX)
            Dim copyH As Integer = Math.Min(_backBuffer.Height - dstY, sprite.Height - srcY)

            If copyW <= 0 OrElse copyH <= 0 Then Return

            ' Lock bitmap for fast pixel access
            Dim bmpData As BitmapData = _backBuffer.LockBits(rectCanvas, ImageLockMode.ReadWrite, _backBuffer.PixelFormat)
            Try
                Dim stride As Integer = bmpData.Stride
                Dim basePtr As IntPtr = bmpData.Scan0

                ' Fast row-by-row copy
                For row As Integer = 0 To copyH - 1
                    Dim targetRowPtr As IntPtr = basePtr + ((dstY + row) * stride) + (dstX * 4)

                    For col As Integer = 0 To copyW - 1
                        Dim px As GraphicsObjects.Pixel = sprite.GetPixel(srcX + col, srcY + row)
                        ' ARGB stored as BGRA in little-endian
                        Dim pxValue As Integer = (CInt(px.A) << 24) Or (CInt(px.R) << 16) Or (CInt(px.G) << 8) Or px.B
                        Marshal.WriteInt32(targetRowPtr, col * 4, pxValue)
                    Next
                Next
            Finally
                _backBuffer.UnlockBits(bmpData)
            End Try
        End Sub

        ''' <summary>
        ''' Fill a rectangle with a solid color
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub FillRect(x As Integer, y As Integer, w As Integer, h As Integer, color As GraphicsObjects.Pixel)
            ' Clamp to canvas bounds
            x = Math.Max(0, Math.Min(x, CANVAS_WIDTH))
            y = Math.Max(0, Math.Min(y, CANVAS_HEIGHT))

            Dim x2 As Integer = Math.Max(0, Math.Min(x + w, CANVAS_WIDTH))
            Dim y2 As Integer = Math.Max(0, Math.Min(y + h, CANVAS_HEIGHT))

            If x >= x2 OrElse y >= y2 Then Return

            Using g As Graphics = Graphics.FromImage(_backBuffer)
                Using brush As New SolidBrush(color.ToColor())
                    g.FillRectangle(brush, x, y, x2 - x, y2 - y)
                End Using
            End Using
        End Sub
#End Region

#Region "NES-Specific Rendering"
        ''' <summary>
        ''' Render a complete NES frame (game screen + debug views)
        ''' </summary>
        Public Sub RenderFrame(ppu As NativePPU2C02, frameNumber As Integer)
            ' Always draw the main game screen
            DrawSprite(MARGIN, MARGIN, ppu.Screen)

            ' Update pattern tables every 30 frames (performance optimization)
            If _needsPatternRedraw OrElse (frameNumber Mod 30) = 0 Then
                DrawPatternTables(ppu)
                _needsPatternRedraw = False
            End If

            ' Update palette display if needed
            If _needsPaletteRedraw Then
                DrawPaletteSelector(ppu)
                _needsPaletteRedraw = False
            End If
        End Sub

        ''' <summary>
        ''' Draw both pattern tables with current selected palette
        ''' </summary>
        Private Sub DrawPatternTables(ppu As NativePPU2C02)
            Dim xOffset As Integer = GAME_WIDTH + 4

            ' Left pattern table
            DrawSprite(xOffset, MARGIN, ppu.GetPatternTable(0, CByte(_selectedPalette)))

            ' Right pattern table
            DrawSprite(xOffset + PATTERN_SIZE + PATTERN_MARGIN, MARGIN, ppu.GetPatternTable(1, CByte(_selectedPalette)))
        End Sub

        ''' <summary>
        ''' Draw palette selector with swatches
        ''' </summary>
        Private Sub DrawPaletteSelector(ppu As NativePPU2C02)
            Dim xOffset As Integer = GAME_WIDTH + 4
            Dim yOffset As Integer = 132

            ' Clear previous selection highlight
            If _prevSelectedPalette <> _selectedPalette Then
                Dim prevX As Integer = xOffset + 1 + (_prevSelectedPalette * (SWATCH_SIZE * 5))
                FillRect(prevX, yOffset, SWATCH_SIZE * 4, SWATCH_SIZE + 2, GraphicsObjects.PixelColors.DarkGrey)
            End If

            ' Draw current selection highlight
            Dim currX As Integer = xOffset + 1 + (_selectedPalette * (SWATCH_SIZE * 5))
            FillRect(currX, yOffset, SWATCH_SIZE * 4, SWATCH_SIZE + 2, GraphicsObjects.PixelColors.Cyan)

            ' Draw all 8 palettes (4 colors each)
            For p As Integer = 0 To 7
                For s As Integer = 0 To 3
                    Dim swatchX As Integer = xOffset + 1 + p * (SWATCH_SIZE * 5) + s * SWATCH_SIZE
                    Dim swatchY As Integer = yOffset + 1
                    FillRect(swatchX, swatchY, SWATCH_SIZE, SWATCH_SIZE, ppu.GetColorFromPalette(CByte(p), CByte(s)))
                Next
            Next
        End Sub

        ''' <summary>
        ''' Get a clone of the current back buffer for UI display
        ''' </summary>
        Public Function GetDisplayBuffer() As Bitmap
            ' Must clone for thread safety
            Return DirectCast(_backBuffer.Clone(), Bitmap)
        End Function
#End Region

#Region "Utility Methods"
        ''' <summary>
        ''' Save the current frame to a PNG file (for debugging)
        ''' </summary>
        Public Sub SaveFrame(filepath As String)
            Try
                _backBuffer.Save(filepath, ImageFormat.Png)
            Catch ex As Exception
                Debug.WriteLine($"Failed to save frame: {ex.Message}")
            End Try
        End Sub

        ''' <summary>
        ''' Save a specific sprite to a PNG file (for debugging)
        ''' </summary>
        Public Shared Sub SaveSprite(sprite As GraphicsObjects.Sprite, filepath As String)
            If sprite Is Nothing Then Return

            Using bmp As New Bitmap(sprite.Width, sprite.Height, PixelFormat.Format32bppArgb)
                Dim rect As New Rectangle(0, 0, sprite.Width, sprite.Height)
                Dim data As BitmapData = bmp.LockBits(rect, ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)
                Try
                    Dim stride As Integer = data.Stride
                    Dim basePtr As IntPtr = data.Scan0

                    For y As Integer = 0 To sprite.Height - 1
                        Dim rowPtr As IntPtr = New IntPtr(basePtr.ToInt64() + (y * stride))
                        For x As Integer = 0 To sprite.Width - 1
                            Dim px = sprite.GetPixel(x, y)
                            ' BGRA format
                            Dim pxValue As Integer = (CInt(px.A) << 24) Or (CInt(px.R) << 16) Or (CInt(px.G) << 8) Or px.B
                            Marshal.WriteInt32(rowPtr, x * 4, pxValue)
                        Next
                    Next
                Finally
                    bmp.UnlockBits(data)
                End Try
                bmp.Save(filepath, ImageFormat.Png)
            End Using
        End Sub

        ''' <summary>
        ''' Force a full redraw of all elements
        ''' </summary>
        Public Sub ForceRedraw()
            _needsPatternRedraw = True
            _needsPaletteRedraw = True
        End Sub
#End Region

    End Class

End Namespace