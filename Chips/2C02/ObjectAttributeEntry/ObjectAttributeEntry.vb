Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' NES OAM (Object Attribute Memory) sprite entry - 4 bytes per sprite
    ''' Represents one hardware sprite in the NES PPU
    ''' </summary>
    <StructLayout(LayoutKind.Sequential, Pack:=1, Size:=4)>
    Public Structure OAMEntry

        ''' <summary>Y position of sprite (0-239, $FF = off-screen)</summary>
        Public Y As Byte

        ''' <summary>Tile index from pattern table</summary>
        Public TileID As Byte

        ''' <summary>
        ''' Attribute flags:
        ''' Bits 0-1: Palette (0-3)
        ''' Bits 2-4: Unused
        ''' Bit 5: Priority (0=front, 1=behind background)
        ''' Bit 6: Flip horizontally
        ''' Bit 7: Flip vertically
        ''' </summary>
        Public Attributes As Byte

        ''' <summary>X position of sprite (0-255)</summary>
        Public X As Byte

#Region "Constructors"
        ''' <summary>
        ''' Create a sprite entry with specified values
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub New(y As Byte, tileId As Byte, attributes As Byte, x As Byte)
            Me.Y = y
            Me.TileID = tileId
            Me.Attributes = attributes
            Me.X = x
        End Sub

        ''' <summary>
        ''' Create an off-screen sprite (Y = $FF)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function CreateOffScreen() As OAMEntry
            Return New OAMEntry(&HFF, 0, 0, 0)
        End Function
#End Region

#Region "Attribute Bit Access"
        ''' <summary>Get palette index (0-3)</summary>
        Public ReadOnly Property Palette As Byte
            Get
                Return Attributes And &H3
            End Get
        End Property

        ''' <summary>Get priority (True = behind background)</summary>
        Public ReadOnly Property IsBehindBackground As Boolean
            Get
                Return (Attributes And &H20) <> 0
            End Get
        End Property

        ''' <summary>Get horizontal flip flag</summary>
        Public ReadOnly Property IsFlippedHorizontally As Boolean
            Get
                Return (Attributes And &H40) <> 0
            End Get
        End Property

        ''' <summary>Get vertical flip flag</summary>
        Public ReadOnly Property IsFlippedVertically As Boolean
            Get
                Return (Attributes And &H80) <> 0
            End Get
        End Property

        ''' <summary>Check if sprite is off-screen</summary>
        Public ReadOnly Property IsOffScreen As Boolean
            Get
                Return Y >= 239
            End Get
        End Property
#End Region

#Region "Byte-Level Access (for OAM DMA)"
        ''' <summary>
        ''' Get byte at position (0=Y, 1=TileID, 2=Attributes, 3=X)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Function GetByteAt(byteIndex As Integer) As Byte
            Select Case byteIndex And &H3  ' Mask to 0-3
                Case 0 : Return Y
                Case 1 : Return TileID
                Case 2 : Return Attributes
                Case 3 : Return X
                Case Else : Return 0  ' Should never happen
            End Select
        End Function

        ''' <summary>
        ''' Set byte at position (0=Y, 1=TileID, 2=Attributes, 3=X)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetByteAt(byteIndex As Integer, value As Byte)
            Select Case byteIndex And &H3  ' Mask to 0-3
                Case 0 : Y = value
                Case 1 : TileID = value
                Case 2 : Attributes = value
                Case 3 : X = value
            End Select
        End Sub
#End Region

#Region "Utility Methods"
        ''' <summary>
        ''' Fill all fields with the same value (used to clear OAM)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub Fill(value As Byte)
            Y = value
            TileID = value
            Attributes = value
            X = value
        End Sub

        ''' <summary>
        ''' Copy values from another OAM entry
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub CopyFrom(ByRef source As OAMEntry)
            Y = source.Y
            TileID = source.TileID
            Attributes = source.Attributes
            X = source.X
        End Sub

        ''' <summary>
        ''' Clear sprite (set to off-screen)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub Clear()
            Y = &HFF
            TileID = 0
            Attributes = 0
            X = 0
        End Sub

        ''' <summary>
        ''' Set sprite position
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetPosition(x As Byte, y As Byte)
            Me.X = x
            Me.Y = y
        End Sub

        ''' <summary>
        ''' Set palette (0-3)
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetPalette(paletteIndex As Byte)
            Attributes = (Attributes And &HFC) Or (paletteIndex And &H3)
        End Sub

        ''' <summary>
        ''' Set priority flag
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetPriority(behindBackground As Boolean)
            If behindBackground Then
                Attributes = Attributes Or &H20
            Else
                Attributes = Attributes And &HDF
            End If
        End Sub

        ''' <summary>
        ''' Set horizontal flip
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetFlipHorizontal(flip As Boolean)
            If flip Then
                Attributes = Attributes Or &H40
            Else
                Attributes = Attributes And &HBF
            End If
        End Sub

        ''' <summary>
        ''' Set vertical flip
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Sub SetFlipVertical(flip As Boolean)
            If flip Then
                Attributes = Attributes Or &H80
            Else
                Attributes = Attributes And &H7F
            End If
        End Sub
#End Region

#Region "Operators & Overrides"
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Operator =(left As OAMEntry, right As OAMEntry) As Boolean
            Return left.Y = right.Y AndAlso
                   left.TileID = right.TileID AndAlso
                   left.Attributes = right.Attributes AndAlso
                   left.X = right.X
        End Operator

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Operator <>(left As OAMEntry, right As OAMEntry) As Boolean
            Return Not (left = right)
        End Operator

        Public Overrides Function ToString() As String
            Return $"OAM(Y={Y}, ID=${TileID:X2}, Attr=${Attributes:X2}, X={X})"
        End Function

        Public Overrides Function Equals(obj As Object) As Boolean
            If TypeOf obj Is OAMEntry Then
                Return Me = DirectCast(obj, OAMEntry)
            End If
            Return False
        End Function

        Public Overrides Function GetHashCode() As Integer
            Return (Y << 24) Or (TileID << 16) Or (Attributes << 8) Or X
        End Function
#End Region

    End Structure

End Namespace