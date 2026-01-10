Imports System.Reflection
Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem.GraphicsObjects

    ''' <summary>
    ''' Sampling mode for sprite pixel access
    ''' </summary>
    Public Enum SampleMode As Byte
        Clamp = 0      ' Clamp to edges (return transparent outside bounds)
        Wrap = 1       ' Wrap/tile the image
    End Enum

    ''' <summary>
    ''' High-performance 2D sprite/bitmap using Memory(Of T) for zero-copy operations
    ''' </summary>
    Public NotInheritable Class Sprite
        Implements IDisposable

        Private ReadOnly _width As Integer
        Private ReadOnly _height As Integer
        Private ReadOnly _pixels As Memory(Of Pixel)
        Private _sampleMode As SampleMode
        Private _isDisposed As Boolean

#If DEBUG Then
        ' Overdraw tracking for performance analysis
        Private Shared _overdrawCount As Long
        Public Shared ReadOnly Property OverdrawCount As Long
            Get
                Return _overdrawCount
            End Get
        End Property
        Public Shared Sub ResetOverdrawCount()
            _overdrawCount = 0
        End Sub
#End If

        ''' <summary>
        ''' Width of the sprite in pixels
        ''' </summary>
        Public ReadOnly Property Width As Integer
            Get
                Return _width
            End Get
        End Property

        ''' <summary>
        ''' Height of the sprite in pixels
        ''' </summary>
        Public ReadOnly Property Height As Integer
            Get
                Return _height
            End Get
        End Property

        ''' <summary>
        ''' Get/Set the sampling mode for out-of-bounds access
        ''' </summary>
        Public Property SamplingMode As SampleMode
            Get
                Return _sampleMode
            End Get
            Set(value As SampleMode)
                _sampleMode = value
            End Set
        End Property

        ''' <summary>
        ''' Direct pixel access by index (fast)
        ''' </summary>
        Default Public Property Pixels(index As Integer) As Pixel
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _pixels.Span(index)
            End Get
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Set(value As Pixel)
                Dim span = _pixels.Span
                span(index) = value
            End Set
        End Property

        ''' <summary>
        ''' Get read-only access to the pixel data as a span
        ''' </summary>
        Public ReadOnly Property PixelSpan As Memory(Of Pixel)
            Get
                Return _pixels '.Span
            End Get
        End Property

        ''' <summary>
        ''' Get the total number of pixels
        ''' </summary>
        Public ReadOnly Property PixelCount As Integer
            Get
                Return _width * _height
            End Get
        End Property

        ''' <summary>
        ''' Create a new sprite with specified dimensions
        ''' </summary>
        Public Sub New(width As Integer, height As Integer)
            If width <= 0 Then Throw New ArgumentException("Width must be positive", NameOf(width))
            If height <= 0 Then Throw New ArgumentException("Height must be positive", NameOf(height))

            _width = width
            _height = height
            _sampleMode = SampleMode.Clamp

            ' Allocate pixel buffer (zero-initialized)
            _pixels = New Memory(Of Pixel)(New Pixel(width * height - 1) {})
        End Sub

        ''' <summary>
        ''' Create a sprite from existing pixel data (zero-copy if possible)
        ''' </summary>
        Public Sub New(width As Integer, height As Integer, pixelData As Pixel())
            If width <= 0 Then Throw New ArgumentException("Width must be positive", NameOf(width))
            If height <= 0 Then Throw New ArgumentException("Height must be positive", NameOf(height))
            If pixelData Is Nothing Then Throw New ArgumentNullException(NameOf(pixelData))
            If pixelData.Length < width * height Then Throw New ArgumentException("Pixel data too small")

            _width = width
            _height = height
            _sampleMode = SampleMode.Clamp
            _pixels = New Memory(Of Pixel)(pixelData)
        End Sub

        ''' <summary>
        ''' Set a pixel at the specified coordinates (aggressively inlined)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function SetPixel(x As Integer, y As Integer, pixel As Pixel) As Boolean
#If DEBUG Then
            Threading.Interlocked.Increment(_overdrawCount)
#End If
            ' Bounds check
            If x >= 0 AndAlso x < _width AndAlso y >= 0 AndAlso y < _height Then
                '_pixels.Span(y * _width + x) = pixel
                Dim span = _pixels.Span
                span(y * _width + x) = pixel
                Return True
            End If
            Return False
        End Function

        ''' <summary>
        ''' Set a pixel without bounds checking (UNSAFE - use only when bounds are guaranteed)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetPixelUnsafe(x As Integer, y As Integer, pixel As Pixel)
            '_pixels.Span(y * _width + x) = pixel
            Dim span = _pixels.Span
            span(y * _width + x) = pixel
        End Sub

        ''' <summary>
        ''' Get a pixel at the specified coordinates (respects sample mode)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function GetPixel(x As Integer, y As Integer) As Pixel
            Select Case _sampleMode
                Case SampleMode.Clamp
                    ' Clamp mode: return transparent if out of bounds
                    If x >= 0 AndAlso x < _width AndAlso y >= 0 AndAlso y < _height Then
                        Return _pixels.Span(y * _width + x)
                    End If
                    Return PixelColors.Transparent

                Case SampleMode.Wrap
                    ' Wrap mode: tile the image
                    Dim wrappedX = ((x Mod _width) + _width) Mod _width
                    Dim wrappedY = ((y Mod _height) + _height) Mod _height
                    Return _pixels.Span(wrappedY * _width + wrappedX)

                Case Else
                    Return PixelColors.Transparent
            End Select
        End Function

        ''' <summary>
        ''' Get a pixel without bounds checking (UNSAFE - use only when bounds are guaranteed)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function GetPixelUnsafe(x As Integer, y As Integer) As Pixel
            Return _pixels.Span(y * _width + x)
        End Function

        ''' <summary>
        ''' Clear the entire sprite to a specific color
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub Clear(Optional color As Pixel = Nothing)
            If color = Nothing Then color = PixelColors.Transparent
            _pixels.Span.Fill(color)
        End Sub

        ''' <summary>
        ''' Fill a rectangular region with a color
        ''' </summary>
        Public Sub FillRect(x As Integer, y As Integer, width As Integer, height As Integer, color As Pixel)
            ' Clamp to sprite bounds
            Dim x1 = Math.Max(0, x)
            Dim y1 = Math.Max(0, y)
            Dim x2 = Math.Min(_width, x + width)
            Dim y2 = Math.Min(_height, y + height)

            Dim span = _pixels.Span

            For py = y1 To y2 - 1
                Dim rowStart = py * _width
                For px = x1 To x2 - 1
                    span(rowStart + px) = color
                Next
            Next
        End Sub

        ''' <summary>
        ''' Draw another sprite onto this one at the specified position
        ''' </summary>
        Public Sub DrawSprite(x As Integer, y As Integer, source As Sprite, Optional alpha As Byte = 255)
            If source Is Nothing Then Return

            Dim srcSpan = source.PixelSpan
            Dim dstSpan = _pixels.Span

            For sy = 0 To source.Height - 1
                Dim dy = y + sy
                If dy < 0 OrElse dy >= _height Then Continue For

                For sx = 0 To source.Width - 1
                    Dim dx = x + sx
                    If dx < 0 OrElse dx >= _width Then Continue For

                    Dim srcPixel = Pixels(sy * source.Width + sx) 'srcSpan

                    ' Skip transparent pixels
                    If srcPixel.A = 0 Then Continue For

                    Dim dstIndex = dy * _width + dx

                    ' Alpha blending if needed
                    If alpha < 255 OrElse srcPixel.A < 255 Then
                        Dim srcAlpha = CSng(srcPixel.A * alpha) / 65025.0F ' (255 * 255)
                        Dim invAlpha = 1.0F - srcAlpha
                        Dim dstPixel = dstSpan(dstIndex)

                        dstSpan(dstIndex) = New Pixel(
                            CByte(srcPixel.R * srcAlpha + dstPixel.R * invAlpha),
                            CByte(srcPixel.G * srcAlpha + dstPixel.G * invAlpha),
                            CByte(srcPixel.B * srcAlpha + dstPixel.B * invAlpha),
                            255
                        )
                    Else
                        ' Opaque - direct copy
                        dstSpan(dstIndex) = srcPixel
                    End If
                Next
            Next
        End Sub

        ''' <summary>
        ''' Copy pixel data to a byte array (for interop with GDI+/DirectX)
        ''' Format: BGRA (standard Windows format)
        ''' </summary>
        Public Function ToByteArray() As Byte()
            Dim bytes(_width * _height * 4 - 1) As Byte
            Dim span = _pixels.Span
            Dim index = 0

            For i = 0 To span.Length - 1
                Dim p = span(i)
                bytes(index) = p.B
                bytes(index + 1) = p.G
                bytes(index + 2) = p.R
                bytes(index + 3) = p.A
                index += 4
            Next

            Return bytes
        End Function

        ''' <summary>
        ''' Load pixel data from a byte array (BGRA format)
        ''' </summary>
        Public Sub FromByteArray(bytes As Byte())
            If bytes Is Nothing Then Throw New ArgumentNullException(NameOf(bytes))
            If bytes.Length < _width * _height * 4 Then Throw New ArgumentException("Byte array too small")

            Dim span = _pixels.Span
            Dim index = 0

            For i = 0 To span.Length - 1
                span(i) = New Pixel(
                    bytes(index + 2),  ' R
                    bytes(index + 1),  ' G
                    bytes(index),      ' B
                    bytes(index + 3)   ' A
                )
                index += 4
            Next
        End Sub

        ''' <summary>
        ''' Create a deep copy of this sprite
        ''' </summary>
        Public Function Clone() As Sprite
            Dim tmpclone As New Sprite(_width, _height)
            _pixels.Span.CopyTo(tmpclone._pixels.Span)
            tmpclone._sampleMode = _sampleMode
            Return tmpclone
        End Function

        Public Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                _isDisposed = True
            End If
        End Sub

        Public Overrides Function ToString() As String
            Return $"Sprite({_width}x{_height}, {PixelCount:N0} pixels)"
        End Function

    End Class

End Namespace