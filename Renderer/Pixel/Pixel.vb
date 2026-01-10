Imports System.Runtime.InteropServices
Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem.GraphicsObjects

    ''' <summary>
    ''' High-performance pixel structure with union access (BGRA format)
    ''' Layout: [AA RR GG BB] in memory (little-endian)
    ''' </summary>
    <StructLayout(LayoutKind.Explicit, Size:=4, Pack:=1)>
    Public Structure Pixel
        ' Individual color components (BGRA order for Windows)
        <FieldOffset(0)> Public B As Byte    ' Blue
        <FieldOffset(1)> Public G As Byte    ' Green
        <FieldOffset(2)> Public R As Byte    ' Red
        <FieldOffset(3)> Public A As Byte    ' Alpha

        ' Full 32-bit access
        <FieldOffset(0)> Public BGRA As UInt32   ' Unsigned access
        <FieldOffset(0)> Public Value As Int32   ' Signed access (for APIs that need it)

        ''' <summary>
        ''' Create a pixel from RGBA components (default opaque)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub New(r As Byte, g As Byte, b As Byte, Optional a As Byte = 255)
            ' Must initialize all fields in a structure constructor
            Me.BGRA = 0
            Me.Value = 0

            Me.R = r
            Me.G = g
            Me.B = b
            Me.A = a
        End Sub

        ''' <summary>
        ''' Create a pixel from a 32-bit color value (BGRA format)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub New(bgraValue As UInt32)
            Me.R = 0
            Me.G = 0
            Me.B = 0
            Me.A = 0
            Me.Value = 0

            Me.BGRA = bgraValue
        End Sub

        ''' <summary>
        ''' Create from RGB hex value (e.g., 0xFF0000 for red)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function FromRGB(rgb As UInt32, Optional alpha As Byte = 255) As Pixel
            Return New Pixel(
                CByte((rgb >> 16) And &HFF),  ' R
                CByte((rgb >> 8) And &HFF),   ' G
                CByte(rgb And &HFF),          ' B
                alpha
            )
        End Function

        ''' <summary>
        ''' Convert to RGB hex value (ignores alpha)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function ToRGB() As UInt32
            Return (CUInt(R) << 16) Or (CUInt(G) << 8) Or B
        End Function

        ''' <summary>
        ''' Check if pixel is transparent
        ''' </summary>
        Public ReadOnly Property IsTransparent As Boolean
            Get
                Return A = 0
            End Get
        End Property

        ''' <summary>
        ''' Check if pixel is opaque
        ''' </summary>
        Public ReadOnly Property IsOpaque As Boolean
            Get
                Return A = 255
            End Get
        End Property

        ''' <summary>
        ''' Linear interpolation between two pixels
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function Lerp(p1 As Pixel, p2 As Pixel, t As Single) As Pixel
            Dim invT = 1.0F - t
            Return New Pixel(
                CByte(p1.R * invT + p2.R * t),
                CByte(p1.G * invT + p2.G * t),
                CByte(p1.B * invT + p2.B * t),
                CByte(p1.A * invT + p2.A * t)
            )
        End Function

        ' Equality operators
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Operator =(p1 As Pixel, p2 As Pixel) As Boolean
            Return p1.BGRA = p2.BGRA
        End Operator

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Operator <>(p1 As Pixel, p2 As Pixel) As Boolean
            Return p1.BGRA <> p2.BGRA
        End Operator

        ' Implicit conversions
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Widening Operator CType(value As UInt32) As Pixel
            Return New Pixel(value)
        End Operator

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Widening Operator CType(p As Pixel) As UInt32
            Return p.BGRA
        End Operator

        Public Overrides Function ToString() As String
            Return $"RGBA({R}, {G}, {B}, {A}) [0x{BGRA:X8}]"
        End Function

        Public Overrides Function Equals(obj As Object) As Boolean
            If TypeOf obj Is Pixel Then
                Return Me = DirectCast(obj, Pixel)
            End If
            Return False
        End Function

        Public Overrides Function GetHashCode() As Integer
            Return Value.GetHashCode()
        End Function

    End Structure

    ''' <summary>
    ''' Pre-defined color constants
    ''' </summary>
    Public Module PixelColors
        ' Standards
        Public ReadOnly Property Black As Pixel = New Pixel(0, 0, 0)
        Public ReadOnly Property White As Pixel = New Pixel(255, 255, 255)
        Public ReadOnly Property Red As Pixel = New Pixel(255, 0, 0)
        Public ReadOnly Property Green As Pixel = New Pixel(0, 255, 0)
        Public ReadOnly Property Blue As Pixel = New Pixel(0, 0, 255)
        Public ReadOnly Property Yellow As Pixel = New Pixel(255, 255, 0)
        Public ReadOnly Property Cyan As Pixel = New Pixel(0, 255, 255)
        Public ReadOnly Property Magenta As Pixel = New Pixel(255, 0, 255)
        Public ReadOnly Property Transparent As Pixel = New Pixel(0, 0, 0, 0)

        ' Grays
        Public ReadOnly Property Grey As Pixel = New Pixel(192, 192, 192)
        Public ReadOnly Property DarkGrey As Pixel = New Pixel(128, 128, 128)
        Public ReadOnly Property VeryDarkGrey As Pixel = New Pixel(64, 64, 64)

        ' Darks
        Public ReadOnly Property DarkRed As Pixel = New Pixel(128, 0, 0)
        Public ReadOnly Property VeryDarkRed As Pixel = New Pixel(64, 0, 0)
        Public ReadOnly Property DarkGreen As Pixel = New Pixel(0, 128, 0)
        Public ReadOnly Property VeryDarkGreen As Pixel = New Pixel(0, 64, 0)
        Public ReadOnly Property DarkBlue As Pixel = New Pixel(0, 0, 128)
        Public ReadOnly Property VeryDarkBlue As Pixel = New Pixel(0, 0, 64)
        Public ReadOnly Property DarkYellow As Pixel = New Pixel(128, 128, 0)
        Public ReadOnly Property VeryDarkYellow As Pixel = New Pixel(64, 64, 0)
        Public ReadOnly Property DarkCyan As Pixel = New Pixel(0, 128, 128)
        Public ReadOnly Property VeryDarkCyan As Pixel = New Pixel(0, 64, 64)
        Public ReadOnly Property DarkMagenta As Pixel = New Pixel(128, 0, 128)
        Public ReadOnly Property VeryDarkMagenta As Pixel = New Pixel(64, 0, 64)
    End Module

End Namespace