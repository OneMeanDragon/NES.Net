Imports System.Collections.Generic
Imports Nintendo.NintendoEntertainmentSystem

Namespace Core

    ''' <summary>
    ''' Simple bitmap font for text rendering
    ''' </summary>
    Public Class SimpleFont
        Implements IDisposable
        Private _isDisposed As Boolean
        Public Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                ClearCharacterCache()
                _isDisposed = True
            End If
        End Sub

        Public Shared ReadOnly Property Characters As Dictionary(Of Char, (Width As Integer, Pixels As Boolean(,)))

        Shared Sub New()
            Characters = New Dictionary(Of Char, (Width As Integer, Pixels As Boolean(,)))()
            InitializeFont()
        End Sub

        Private Shared Sub InitializeFont()
            ' Define a simple 5x7 pixel font
            ' Each character is defined as a 2D boolean array where True = pixel on

            ' Space (3px wide, no pixels)
            Characters(" "c) = (3, New Boolean(5, 2) {})

            ' Numbers 0-9
            Characters("0"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, True, True},
                {True, False, True, False, True},
                {True, True, False, False, True},
                {False, True, True, True, False}
            })

            Characters("1"c) = (3, New Boolean(,) {
                {False, True, False},
                {True, True, False},
                {False, True, False},
                {False, True, False},
                {False, True, False},
                {True, True, True}
            })

            Characters("2"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {False, False, False, False, True},
                {False, False, True, True, False},
                {False, True, False, False, False},
                {True, True, True, True, True}
            })

            Characters("3"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {False, False, False, True, False},
                {False, False, False, False, True},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("4"c) = (5, New Boolean(,) {
                {False, False, False, True, False},
                {False, False, True, True, False},
                {False, True, False, True, False},
                {True, False, False, True, False},
                {True, True, True, True, True},
                {False, False, False, True, False}
            })

            Characters("5"c) = (5, New Boolean(,) {
                {True, True, True, True, True},
                {True, False, False, False, False},
                {True, True, True, True, False},
                {False, False, False, False, True},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("6"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, False},
                {True, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("7"c) = (5, New Boolean(,) {
                {True, True, True, True, True},
                {False, False, False, False, True},
                {False, False, False, True, False},
                {False, False, True, False, False},
                {False, True, False, False, False},
                {True, False, False, False, False}
            })

            Characters("8"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("9"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {False, True, True, True, True},
                {False, False, False, False, True},
                {False, True, True, True, False}
            })

            ' Uppercase letters A-Z
            Characters("A"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, True, True, True, True},
                {True, False, False, False, True},
                {True, False, False, False, True}
            })

            Characters("B"c) = (5, New Boolean(,) {
                {True, True, True, True, False},
                {True, False, False, False, True},
                {True, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, True, True, True, False}
            })

            Characters("C"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, False},
                {True, False, False, False, False},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("D"c) = (5, New Boolean(,) {
                {True, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, True, True, True, False}
            })

            Characters("E"c) = (5, New Boolean(,) {
                {True, True, True, True, True},
                {True, False, False, False, False},
                {True, True, True, True, False},
                {True, False, False, False, False},
                {True, False, False, False, False},
                {True, True, True, True, True}
            })

            Characters("F"c) = (5, New Boolean(,) {
                {True, True, True, True, True},
                {True, False, False, False, False},
                {True, True, True, True, False},
                {True, False, False, False, False},
                {True, False, False, False, False},
                {True, False, False, False, False}
            })

            Characters("G"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, False},
                {True, False, True, True, True},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("H"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, True, True, True, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True}
            })

            Characters("I"c) = (3, New Boolean(,) {
                {True, True, True},
                {False, True, False},
                {False, True, False},
                {False, True, False},
                {False, True, False},
                {True, True, True}
            })

            Characters("J"c) = (5, New Boolean(,) {
                {False, False, False, False, True},
                {False, False, False, False, True},
                {False, False, False, False, True},
                {False, False, False, False, True},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("K"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, False, False, True, False},
                {True, False, True, False, False},
                {True, True, False, False, False},
                {True, False, True, False, False},
                {True, False, False, True, False}
            })

            Characters("L"c) = (5, New Boolean(,) {
                {True, False, False, False, False},
                {True, False, False, False, False},
                {True, False, False, False, False},
                {True, False, False, False, False},
                {True, False, False, False, False},
                {True, True, True, True, True}
            })

            Characters("M"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, True, False, True, True},
                {True, False, True, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True}
            })

            Characters("N"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, True, False, False, True},
                {True, False, True, False, True},
                {True, False, False, True, True},
                {True, False, False, False, True},
                {True, False, False, False, True}
            })

            Characters("O"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("P"c) = (5, New Boolean(,) {
                {True, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, True, True, True, False},
                {True, False, False, False, False},
                {True, False, False, False, False}
            })

            Characters("Q"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, True, False, True},
                {True, False, False, True, False},
                {False, True, True, False, True}
            })

            Characters("R"c) = (5, New Boolean(,) {
                {True, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, True, True, True, False},
                {True, False, True, False, False},
                {True, False, False, True, False}
            })

            Characters("S"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, False, False, False},
                {False, True, True, True, False},
                {False, False, False, False, True},
                {True, True, True, True, False}
            })

            Characters("T"c) = (5, New Boolean(,) {
                {True, True, True, True, True},
                {False, False, True, False, False},
                {False, False, True, False, False},
                {False, False, True, False, False},
                {False, False, True, False, False},
                {False, False, True, False, False}
            })

            Characters("U"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {False, True, True, True, False}
            })

            Characters("V"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {False, True, False, True, False},
                {False, False, True, False, False}
            })

            Characters("W"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, False, False, True},
                {True, False, True, False, True},
                {True, True, False, True, True},
                {True, False, False, False, True}
            })

            Characters("X"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, False, False, False, True},
                {False, True, False, True, False},
                {False, False, True, False, False},
                {False, True, False, True, False},
                {True, False, False, False, True}
            })

            Characters("Y"c) = (5, New Boolean(,) {
                {True, False, False, False, True},
                {True, False, False, False, True},
                {False, True, False, True, False},
                {False, False, True, False, False},
                {False, False, True, False, False},
                {False, False, True, False, False}
            })

            Characters("Z"c) = (5, New Boolean(,) {
                {True, True, True, True, True},
                {False, False, False, False, True},
                {False, False, False, True, False},
                {False, False, True, False, False},
                {False, True, False, False, False},
                {True, True, True, True, True}
            })

            ' Special characters
            Characters(":"c) = (1, New Boolean(,) {
                {False},
                {True},
                {False},
                {False},
                {True},
                {False}
            })

            Characters(";"c) = (2, New Boolean(,) {
                {False, False},
                {False, True},
                {False, False},
                {False, False},
                {False, True},
                {True, False}
            })

            Characters("-"c) = (3, New Boolean(,) {
                {False, False, False},
                {False, False, False},
                {False, False, False},
                {True, True, True},
                {False, False, False},
                {False, False, False}
            })

            Characters("_"c) = (5, New Boolean(,) {
                {False, False, False, False, False},
                {False, False, False, False, False},
                {False, False, False, False, False},
                {False, False, False, False, False},
                {False, False, False, False, False},
                {True, True, True, True, True}
            })

            Characters("."c) = (1, New Boolean(,) {
                {False},
                {False},
                {False},
                {False},
                {False},
                {True}
            })

            Characters(","c) = (2, New Boolean(,) {
                {False, False},
                {False, False},
                {False, False},
                {False, False},
                {False, True},
                {True, False}
            })

            Characters("!"c) = (1, New Boolean(,) {
                {True},
                {True},
                {True},
                {True},
                {False},
                {True}
            })

            Characters("?"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {False, False, False, True, False},
                {False, False, True, False, False},
                {False, False, False, False, False},
                {False, False, True, False, False}
            })

            Characters("'"c) = (1, New Boolean(,) {
                {True},
                {True},
                {False},
                {False},
                {False},
                {False}
            })

            Characters(""""c) = (3, New Boolean(,) {
                {True, False, True},
                {True, False, True},
                {False, False, False},
                {False, False, False},
                {False, False, False},
                {False, False, False}
            })

            Characters("+"c) = (3, New Boolean(,) {
                {False, False, False},
                {False, True, False},
                {True, True, True},
                {False, True, False},
                {False, False, False},
                {False, False, False}
            })

            Characters("="c) = (3, New Boolean(,) {
                {False, False, False},
                {True, True, True},
                {False, False, False},
                {True, True, True},
                {False, False, False},
                {False, False, False}
            })

            Characters("*"c) = (3, New Boolean(,) {
                {False, False, False},
                {True, False, True},
                {False, True, False},
                {True, False, True},
                {False, False, False},
                {False, False, False}
            })

            Characters("/"c) = (3, New Boolean(,) {
                {False, False, True},
                {False, False, True},
                {False, True, False},
                {False, True, False},
                {True, False, False},
                {True, False, False}
            })

            Characters("\"c) = (3, New Boolean(,) {
                {True, False, False},
                {True, False, False},
                {False, True, False},
                {False, True, False},
                {False, False, True},
                {False, False, True}
            })

            Characters("("c) = (2, New Boolean(,) {
                {False, True},
                {True, False},
                {True, False},
                {True, False},
                {True, False},
                {False, True}
            })

            Characters(")"c) = (2, New Boolean(,) {
                {True, False},
                {False, True},
                {False, True},
                {False, True},
                {False, True},
                {True, False}
            })

            Characters("["c) = (2, New Boolean(,) {
                {True, True},
                {True, False},
                {True, False},
                {True, False},
                {True, False},
                {True, True}
            })

            Characters("]"c) = (2, New Boolean(,) {
                {True, True},
                {False, True},
                {False, True},
                {False, True},
                {False, True},
                {True, True}
            })

            Characters("<"c) = (3, New Boolean(,) {
                {False, False, True},
                {False, True, False},
                {True, False, False},
                {True, False, False},
                {False, True, False},
                {False, False, True}
            })

            Characters(">"c) = (3, New Boolean(,) {
                {True, False, False},
                {False, True, False},
                {False, False, True},
                {False, False, True},
                {False, True, False},
                {True, False, False}
            })

            Characters("%"c) = (5, New Boolean(,) {
                {True, True, False, False, True},
                {True, True, False, True, False},
                {False, False, True, False, False},
                {False, True, False, False, False},
                {True, False, True, True, False},
                {False, False, True, True, False}
            })

            Characters("$"c) = (5, New Boolean(,) {
                {False, False, True, False, False},
                {False, True, True, True, True},
                {True, False, True, False, False},
                {False, True, True, True, False},
                {False, False, True, False, True},
                {True, True, True, True, False}
            })

            Characters("#"c) = (5, New Boolean(,) {
                {False, True, False, True, False},
                {False, True, False, True, False},
                {True, True, True, True, True},
                {False, True, False, True, False},
                {True, True, True, True, True},
                {False, True, False, True, False}
            })

            Characters("&"c) = (5, New Boolean(,) {
                {False, True, True, False, False},
                {True, False, False, True, False},
                {False, True, True, False, False},
                {True, False, False, True, False},
                {True, False, False, False, True},
                {False, True, True, False, True}
            })

            Characters("@"c) = (5, New Boolean(,) {
                {False, True, True, True, False},
                {True, False, False, False, True},
                {True, False, True, True, True},
                {True, False, True, False, True},
                {True, False, True, True, False},
                {False, True, True, False, False}
            })

        End Sub

        ''' <summary>
        ''' Measure the width of a string in pixels
        ''' </summary>
        Public Shared Function MeasureString(text As String, spacing As Integer) As Integer
            If String.IsNullOrEmpty(text) Then Return 0

            Dim width As Integer = 0
            For i As Integer = 0 To text.Length - 1
                Dim c As Char = Char.ToUpperInvariant(text(i))
                If Characters.ContainsKey(c) Then
                    width += Characters(c).Width
                    If i < text.Length - 1 Then
                        width += spacing
                    End If
                Else
                    ' Default width for unknown characters
                    width += 5 + spacing
                End If
            Next

            Return width
        End Function

        ''' <summary>
        ''' Get character data for a specific character
        ''' </summary>
        Public Shared Function GetCharacter(c As Char) As (Width As Integer, Pixels As Boolean(,))
            ' Ensure font is initialized
            If Characters Is Nothing OrElse Characters.Count = 0 Then
                Return (3, New Boolean(5, 2) {}) ' Return empty space dimensions
            End If

            Dim upperC As Char = Char.ToUpperInvariant(c)
            If Characters.ContainsKey(upperC) Then
                Return Characters(upperC)
            Else
                ' Return space for unknown characters (with null check)
                If Characters.ContainsKey(" "c) Then
                    Return Characters(" "c)
                Else
                    ' Fallback if even space isn't defined
                    Return (3, New Boolean(5, 2) {})
                End If
            End If
        End Function

#Region "Character Sprite Cache"
        Private _characterCache As New Dictionary(Of String, GraphicsObjects.Sprite)

        ''' <summary>
        ''' Get or create a cached sprite for a character with the given color and shadow
        ''' </summary>
        Public Function GetCachedCharacter(c As Char, color As GraphicsObjects.Pixel, shadow As Boolean) As GraphicsObjects.Sprite
            ' Create unique cache key based on character, color, and shadow
            Dim key As String = $"{c}_{color.BGRA}_{shadow}"

            If _characterCache.ContainsKey(key) Then
                Return _characterCache(key)
            End If

            ' Create new character sprite
            Dim charInfo = SimpleFont.GetCharacter(c)
            Dim pixels = charInfo.Pixels

            If pixels Is Nothing OrElse pixels.Length = 0 OrElse pixels.GetLength(0) = 0 OrElse pixels.GetLength(1) = 0 Then
                Return Nothing
            End If

            ' Calculate sprite dimensions (add 1 pixel for shadow if needed)
            Dim width As Integer = charInfo.Width + If(shadow, 1, 0)
            Dim height As Integer = pixels.GetLength(0) + If(shadow, 1, 0)

            ' Create sprite
            Dim sprite As New GraphicsObjects.Sprite(width, height)

            ' Fill with transparent pixels
            For py As Integer = 0 To height - 1
                For px As Integer = 0 To width - 1
                    sprite.SetPixel(px, py, GraphicsObjects.PixelColors.Transparent)
                Next
            Next

            ' Draw shadow if requested
            If shadow Then
                Dim shadowColor As GraphicsObjects.Pixel = GraphicsObjects.PixelColors.Black
                For py As Integer = 0 To pixels.GetLength(0) - 1
                    For px As Integer = 0 To pixels.GetLength(1) - 1
                        If pixels(py, px) Then
                            sprite.SetPixel(px + 1, py + 1, shadowColor)
                        End If
                    Next
                Next
            End If

            ' Draw character
            For py As Integer = 0 To pixels.GetLength(0) - 1
                For px As Integer = 0 To pixels.GetLength(1) - 1
                    If pixels(py, px) Then
                        sprite.SetPixel(px, py, color)
                    End If
                Next
            Next

            ' Cache and return
            _characterCache(key) = sprite
            Return sprite
        End Function

        ''' <summary>
        ''' Clear the character cache (call when memory needs to be freed)
        ''' </summary>
        Public Sub ClearCharacterCache()
            For Each sprite In _characterCache.Values
                sprite?.Dispose()
            Next
            _characterCache.Clear()
        End Sub
#End Region

    End Class

End Namespace