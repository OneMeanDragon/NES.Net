Imports System.Runtime.CompilerServices
Imports Nintendo.NintendoEntertainmentSystem.GraphicsObjects

Public Module PixelExtensions

    ''' <summary>
    ''' Extension method: Convert Pixel to Color
    ''' </summary>
    <Extension>
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function ToColor(p As Pixel) As Color
        Return Color.FromArgb(p.A, p.R, p.G, p.B)
    End Function

End Module