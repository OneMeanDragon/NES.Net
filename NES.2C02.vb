
Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices 'For the union type

Namespace MathObjects
    Public Class ByteBitField
        Private Const FLAG_COUNT As UInt32 = 7 '8-1
        Private m_Bit(7) As Boolean
        Private m_Byte As Byte

        Public Sub New()
            For i As Integer = 0 To FLAG_COUNT
                m_Bit(i) = False
            Next
            m_Byte = 0
        End Sub

        ''' <summary>
        ''' SetBit will turn on or off a spacific flag
        ''' </summary>
        ''' <param name="arBit">There is a total of 8 flags starting at 0 ending at 7</param>
        Public WriteOnly Property SetBit(ByVal arBit As UInt32)
            Set(value)
                If arBit > 7 Then Return
                If m_Bit(arBit) Then
                    ' Remove
                    m_Byte = m_Byte Xor (1 << arBit)
                    m_Bit(arBit) = False
                Else
                    ' Add
                    m_Byte = m_Byte Or (1 << arBit)
                    m_Bit(arBit) = True
                End If
            End Set
        End Property
    End Class
End Namespace

Namespace GraphicsObjects
#Const OVERDRAW = 0
    Public Class Sprite 'Rect
        Public Property Width() As UInt32
        Public Property Height() As UInt32
        Public Enum enuMode
            normal
            periodic
        End Enum
        Private modeSample As enuMode

        Public pColData() As Pixel

        Public Sub New(ByVal arWidth As UInt32, ByVal arHeight As UInt32)
            modeSample = enuMode.normal
            Width = arWidth
            Height = arHeight
            ReDim pColData((Width * Height) - 1)
            For i As UInt32 = 0 To ((Width * Height) - 1) 'VB
                pColData(i) = New Pixel()
            Next
        End Sub
        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub


#If (OVERDRAW >= 1) Then
        Public Shared nOverdrawCount As UInt32
#End If
        Public Function SetPixel(ByVal x As Int32, ByVal y As Int32, ByVal p As Pixel) As Boolean
#If (OVERDRAW >= 1) Then
            nOverdrawCount += 1
#End If
            If x >= 0 AndAlso x < Width AndAlso y >= 0 AndAlso y < Height Then
                pColData(y * Width + x).Copy(p)
                Return True
            End If
            Return False
        End Function

        Public Function GetPixel(ByVal x As Int32, ByVal y As Int32) As Pixel
            If modeSample = enuMode.normal Then
                If x >= 0 AndAlso x < Width AndAlso y >= 0 AndAlso y < Height Then
                    Return pColData(y * Width + x)
                Else
                    Return New Pixel(0, 0, 0, 0)
                End If
            Else
                Return pColData(Math.Abs(y Mod Height) * Width + Math.Abs(x Mod Width))
            End If
        End Function
    End Class

    <StructLayout(LayoutKind.Explicit)>
    Public Structure PixelUnion
        <FieldOffset(2)> Public r As Byte
        <FieldOffset(1)> Public g As Byte
        <FieldOffset(0)> Public b As Byte
        <FieldOffset(3)> Public a As Byte
        <FieldOffset(0)> Public n As UInt32 ' = &HFF000000UI
        <FieldOffset(0)> Public Signed As Int32
    End Structure 'I may have these fields in reverse order? (apparently not acording to c++)

#Region "Bordom UInt24"

    Public Class UInt24
        <StructLayout(LayoutKind.Explicit)>
        Private Structure I24_Struct
            <FieldOffset(0)> Public b1 As Byte
            <FieldOffset(1)> Public b2 As Byte
            <FieldOffset(2)> Public b3 As Byte
            <FieldOffset(3)> Public b4 As Byte
            <FieldOffset(0)> Public n As UInt32 ' = &H00000000UI
        End Structure
        Private Union24 As I24_Struct
        Public Sub New(ByVal b1 As Byte, ByVal b2 As Byte, ByVal b3 As Byte)
            Union24.b1 = b1
            Union24.b2 = b2
            Union24.b3 = b3
            Union24.b4 = 0
        End Sub
        Public Sub New(ByVal value As UInt32)
            If value > &HFFFFFFUI Then Throw New OverflowException("UInt24 has Overflowed")
            Union24.n = value
        End Sub


    End Class

#End Region

    Public Class Pixel
        Public Enum enMode
            normal
            masking
            alpha
            custom
        End Enum
        Public Mode As enMode = enMode.normal
        Public m_Pixel As PixelUnion

        Public Sub New()
            m_Pixel.n = &HFF000000UI 'Default Black
        End Sub
        Public Sub New(ByVal r As Byte, ByVal g As Byte, ByVal b As Byte, ByVal Optional a As Byte = &HFFUI)
            '----------------------------------------------------.
            m_Pixel.r = r                                       '|
            m_Pixel.g = g                                       '|
            m_Pixel.b = b                                       '|
            m_Pixel.a = a                                       '|
            '---------------------------------------------------'|
            'm_Pixel.n = r or (g << 8) or (b << 16) or (a << 24)'|
            '----------------------------------------------------'
        End Sub
        Public Sub New(ByVal iValue As UInt32)
            m_Pixel.n = iValue
        End Sub

        Public Sub Copy(ByVal p As Pixel)
            Me.Mode = p.Mode
            Me.m_Pixel.a = p.m_Pixel.a
            Me.m_Pixel.r = p.m_Pixel.r
            Me.m_Pixel.g = p.m_Pixel.g
            Me.m_Pixel.b = p.m_Pixel.b
            'If Me.m_Pixel.n <> p.m_Pixel.n Then
            '    Debug.WriteLine("Failed to copy pixel.")
            'End If
        End Sub

        Public Shared Operator =(ByVal p1 As Pixel, ByVal p2 As Pixel) As Boolean
            Return (p1.m_Pixel.n = p2.m_Pixel.n)
        End Operator

        Public Shared Operator <>(ByVal p1 As Pixel, ByVal p2 As Pixel) As Boolean
            Return (p1.m_Pixel.n <> p2.m_Pixel.n)
        End Operator
    End Class

    Structure PixelColors
        Public Shared GREY As New Pixel(192, 192, 192)
        Public Shared DARK_GREY As New Pixel(128, 128, 128)
        Public Shared VERY_DARK_GREY As New Pixel(64, 64, 64)
        Public Shared RED As New Pixel(255, 0, 0)
        Public Shared DARK_RED As New Pixel(128, 0, 0)
        Public Shared VERY_DARK_RED As New Pixel(64, 0, 0)
        Public Shared YELLOW As New Pixel(255, 255, 0)
        Public Shared DARK_YELLOW As New Pixel(128, 128, 0)
        Public Shared VERY_DARK_YELLOW As New Pixel(64, 64, 0)
        Public Shared GREEN As New Pixel(0, 255, 0)
        Public Shared DARK_GREEN As New Pixel(0, 128, 0)
        Public Shared VERY_DARK_GREEN As New Pixel(0, 64, 0)
        Public Shared CYAN As New Pixel(0, 255, 255)
        Public Shared DARK_CYAN As New Pixel(0, 128, 128)
        Public Shared VERY_DARK_CYAN As New Pixel(0, 64, 64)
        Public Shared BLUE As New Pixel(0, 0, 255)
        Public Shared DARK_BLUE As New Pixel(0, 0, 128)
        Public Shared VERY_DARK_BLUE As New Pixel(0, 0, 64)
        Public Shared MAGENTA As New Pixel(255, 0, 255)
        Public Shared DARK_MAGENTA As New Pixel(128, 0, 128)
        Public Shared VERY_DARK_MAGENTA As New Pixel(64, 0, 64)
        Public Shared WHITE As New Pixel(255, 255, 255)
        Public Shared BLACK As New Pixel(0, 0, 0)
        Public Shared BLANK As New Pixel(0, 0, 0, 0)
    End Structure

End Namespace

Namespace NintendoEntertainmentSystem
    Public Module Flags_8Bit_Byte
        Public Const BitFlag0 As UInteger = (1 << 0)
        Public Const BitFlag1 As UInteger = (1 << 1)
        Public Const BitFlag2 As UInteger = (1 << 2)
        Public Const BitFlag3 As UInteger = (1 << 3)
        Public Const BitFlag4 As UInteger = (1 << 4)
        Public Const BitFlag5 As UInteger = (1 << 5)
        Public Const BitFlag6 As UInteger = (1 << 6)
        Public Const BitFlag7 As UInteger = (1 << 7)
    End Module

    Public Class em2C02

        Public tblName(1, 1023) As Byte 'VB
        Private tblPalette(31) As Byte 'VB
        Private tblPattern(1, 4095) As Byte 'VB

        Public palScreen(&H40UI) As GraphicsObjects.Pixel
        Private sprScreen As New GraphicsObjects.Sprite(256, 240) 'Screen
        Private sprNameTable(1) As GraphicsObjects.Sprite       'Set the values in the constructor 'VB
        Private sprPatternTable(1) As GraphicsObjects.Sprite    'Set the values in the constructor 'VB

        ' Pixel offset horizont
        Private fine_x As Byte = &H0UI

        ' Internal communications
        Private address_latch As Byte = &H0UI
        Private ppu_data_buffer As Byte = &H0UI

        ' Pixel "dot" position information
        Private scanline As UInt16 = 0
        Private cycle As Int16 = 0
        Private odd_frame As Boolean = False

        ' Background rendering
        Private bg_next_tile_id As Byte = 0
        Private bg_next_tile_attrib As Byte = 0
        Private bg_next_tile_lsb As Byte = 0
        Private bg_next_tile_msb As Byte = 0
        Private bg_shifter_pattern_lo As UInt16 = 0
        Private bg_shifter_pattern_hi As UInt16 = 0
        Private bg_shifter_attrib_lo As UInt16 = 0
        Private bg_shifter_attrib_hi As UInt16 = 0

        Public nmi As Boolean = False
        Public scanline_trigger As Boolean = False


        '// Foreground "Sprite" rendering ================================
        '// The OAM Is an additional memory internal to the PPU. It Is
        '// Not connected via the any bus. It stores the locations of
        '// 64off 8x8 (Or 8x16) tiles to be drawn on the next frame.
        Public Structure sObjectAttributeEntry
            Public y As Byte            ' Y position of sprite
            Public id As Byte           ' ID of tile from pattern memory
            Public attribute As Byte    ' Flags define how sprite should be rendered
            Public x As Byte            ' X position of sprite
            Public Function GetByteAt(ByVal position_index As UInt16) As Byte
                Select Case (position_index Mod 4)
                    Case 0 : Return y
                    Case 1 : Return id
                    Case 2 : Return attribute
                    Case 3 : Return x
                    Case Else
                        Debug.WriteLine("GetByteAt out of bounds: " & position_index.ToString())
                        Return 0 'should throw an error but whatever
                End Select
            End Function
            Public Sub SetByteAt(ByVal position_index As UInt16, ByVal data As Byte)
                Select Case (position_index Mod 4)
                    Case 0 : y = data : Return
                    Case 1 : id = data : Return
                    Case 2 : attribute = data : Return
                    Case 3 : x = data : Return
                    Case Else
                        Debug.WriteLine("SetByteAt out of bounds: " & position_index.ToString())
                        Return 'should throw an error but whatever
                End Select
            End Sub
            Public Sub MemorySet(ByVal bytFill As Byte)
                y = bytFill
                id = bytFill
                attribute = bytFill
                x = bytFill
            End Sub
            Public Sub Copy(ByVal objCpy As sObjectAttributeEntry)
                y = objCpy.y
                id = objCpy.id
                attribute = objCpy.attribute
                x = objCpy.x
            End Sub
        End Structure
        Public OAM(63) As sObjectAttributeEntry 'VB
        '[OAM[address][address]=OAM(address \ 4).G/SetByteAt(address)]
        'Suppose could have just made this an array of 256 bytes heh

        '// A register to store the address when the CPU manually communicates
        '// with OAM via PPU registers. This Is Not commonly used because it 
        '// Is very slow, And instead a 256-Byte DMA transfer Is used. See
        '// the Bus header for a description of this.
        Private oam_addr As Byte = 0

        Private spriteScanline(7) As sObjectAttributeEntry 'VB 8
        Private sprite_count As Byte = 0
        Private sprite_shifter_pattern_lo(7) As Byte 'VB 8
        Private sprite_shifter_pattern_hi(7) As Byte 'VB 8

        ' Sprite zero collision flags
        Private bSpriteZeroHitPossible As Boolean = False
        Private bSpriteZeroBeingRendered As Boolean = False

        '// The OAM Is conveniently package above to work with, but the DMA
        '// mechanism will need access to it for writing one byute at a time
        'public: uint8_t* pOAM = (uint8_t*)OAM 
        '--------------------------------------------.
        ' Calculated as such                         |
        ' OAM(oam_addr \ 4).GetByteAt(oam_addr Mod 4)|
        '--------------------------------------------'

        Public frame_complete As Boolean = False

        Public Class FrankenStatus
            Private BitField(7) As Boolean '8-bit union

            Public Sub New()
                For i As Integer = 0 To 7
                    BitField(i) = False
                Next
            End Sub
            Protected Overrides Sub Finalize()
                MyBase.Finalize()
            End Sub


            Private m_nametable_x As Byte = 0
            Public Property unused() As Byte 'handles 5 fields
                Get
                    Return m_nametable_x
                End Get
                Set(value As Byte)
                    For i As Integer = 0 To 4
                        If value And (1 << i) Then
                            BitField(0) = True
                        Else
                            BitField(0) = False
                        End If
                    Next
                    m_nametable_x = value And &H1F
                End Set
            End Property
            Private m_sprite_size As Byte = 0
            Public Property sprite_overflow() As Byte
                Get
                    Return m_sprite_size
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(5) = True
                    Else
                        BitField(5) = False
                    End If
                    m_sprite_size = value And 1
                End Set
            End Property
            Private m_slave_mode As Byte = 0
            Public Property sprite_zero_hit() As Byte
                Get
                    Return m_slave_mode
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(6) = True
                    Else
                        BitField(6) = False
                    End If
                    m_sprite_size = value And 1
                End Set
            End Property
            Private m_enable_nmi As Byte = 0
            Public Property vertical_blank() As Byte
                Get
                    Return m_enable_nmi
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(7) = True
                    Else
                        BitField(7) = False
                    End If
                    m_enable_nmi = value And 1
                End Set
            End Property
            Public Property reg() As Byte
                Get
                    Dim ReturnValue As Byte = 0
                    For i As Integer = 0 To 7
                        If BitField(i) Then
                            ReturnValue += (1 << i)
                        End If
                    Next
                    Return ReturnValue
                End Get
                Set(value As Byte)
                    For i As Integer = 0 To 7
                        If value And (1 << i) Then
                            BitField(i) = True
                        Else
                            BitField(i) = False
                        End If
                    Next
                    'Set the values
                    m_nametable_x = value And &H1F
                    m_sprite_size = (value And (1 << 5)) >> 5
                    m_slave_mode = (value And (1 << 6)) >> 6
                    m_enable_nmi = (value And (1 << 7)) >> 7
                End Set
            End Property

        End Class
        'Public Structure FrankenStatus
        '    Private m_unused_1 As Byte ' bit 0
        '    Private m_unused_2 As Byte ' bit 1
        '    Private m_unused_3 As Byte ' bit 2
        '    Private m_unused_4 As Byte ' bit 3
        '    Private m_unused_5 As Byte ' bit 4

        '    Private m_sprite_overflow As Byte ' bit 5
        '    Private m_sprite_zero_hit As Byte ' bit 6
        '    Private m_vertical_blank As Byte ' bit 7

        '    Private n_TotalByte As Byte

        '    Private Sub Reset()
        '        m_unused_1 = 0
        '        m_unused_2 = 0
        '        m_unused_3 = 0
        '        m_unused_4 = 0
        '        m_unused_5 = 0
        '        m_sprite_overflow = 0
        '        m_sprite_zero_hit = 0
        '        m_vertical_blank = 0
        '        n_TotalByte = 0
        '    End Sub

        '    Public Property unused1() As Byte
        '        Get
        '            Return m_unused_1
        '        End Get
        '        Set(value As Byte)
        '            m_unused_1 = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag0) = BitFlag0)
        '            If m_unused_1 Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag0
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag0
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property unused2() As Byte
        '        Get
        '            Return m_unused_2
        '        End Get
        '        Set(value As Byte)
        '            m_unused_2 = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag1) = BitFlag1)
        '            If m_unused_2 Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag1
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag1
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property unused3() As Byte
        '        Get
        '            Return m_unused_3
        '        End Get
        '        Set(value As Byte)
        '            m_unused_3 = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag2) = BitFlag2)
        '            If m_unused_3 Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag2
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag2
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property unused4() As Byte
        '        Get
        '            Return m_unused_4
        '        End Get
        '        Set(value As Byte)
        '            m_unused_4 = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag3) = BitFlag3)
        '            If m_unused_4 Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag3
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag3
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property unused5() As Byte
        '        Get
        '            Return m_unused_5
        '        End Get
        '        Set(value As Byte)
        '            m_unused_5 = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag4) = BitFlag4)
        '            If m_unused_5 Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag4
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag4
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property sprite_overflow() As Byte
        '        Get
        '            Return m_sprite_overflow
        '        End Get
        '        Set(value As Byte)
        '            m_sprite_overflow = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag5) = BitFlag5)
        '            If m_sprite_overflow Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag5
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag5
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property sprite_zero_hit() As Byte
        '        Get
        '            Return m_sprite_zero_hit
        '        End Get
        '        Set(value As Byte)
        '            m_sprite_zero_hit = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag6) = BitFlag6)
        '            If m_sprite_zero_hit Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag6
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag6
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property vertical_blank() As Byte
        '        Get
        '            Return m_vertical_blank
        '        End Get
        '        Set(value As Byte)
        '            m_vertical_blank = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag7) = BitFlag7)
        '            If m_vertical_blank Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag7
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag7
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property reg() As Byte
        '        Get
        '            Return n_TotalByte
        '        End Get
        '        Set(value As Byte)
        '            Reset()
        '            If (value And BitFlag0) = BitFlag0 Then n_TotalByte = n_TotalByte Or BitFlag0 : m_unused_1 = 1
        '            If (value And BitFlag1) = BitFlag1 Then n_TotalByte = n_TotalByte Or BitFlag1 : m_unused_2 = 1
        '            If (value And BitFlag2) = BitFlag2 Then n_TotalByte = n_TotalByte Or BitFlag2 : m_unused_3 = 1
        '            If (value And BitFlag3) = BitFlag3 Then n_TotalByte = n_TotalByte Or BitFlag3 : m_unused_4 = 1
        '            If (value And BitFlag4) = BitFlag4 Then n_TotalByte = n_TotalByte Or BitFlag4 : m_unused_5 = 1
        '            If (value And BitFlag5) = BitFlag5 Then n_TotalByte = n_TotalByte Or BitFlag5 : m_sprite_overflow = 1
        '            If (value And BitFlag6) = BitFlag6 Then n_TotalByte = n_TotalByte Or BitFlag6 : m_sprite_zero_hit = 1
        '            If (value And BitFlag7) = BitFlag7 Then n_TotalByte = n_TotalByte Or BitFlag7 : m_vertical_blank = 1
        '        End Set
        '    End Property
        'End Structure
        Private PPUStatus As New FrankenStatus

        Public Class FrankenMask
            Private BitField(7) As Boolean '8-bit union

            Public Sub New()
                For i As Integer = 0 To 7
                    BitField(i) = False
                Next
            End Sub
            Protected Overrides Sub Finalize()
                MyBase.Finalize()
            End Sub


            Private m_nametable_x As Byte = 0
            Public Property grayscale() As Byte
                Get
                    Return m_nametable_x
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(0) = True
                    Else
                        BitField(0) = False
                    End If
                    m_nametable_x = value And 1
                End Set
            End Property
            Private m_nametable_y As Byte = 0
            Public Property render_background_left() As Byte
                Get
                    Return m_nametable_y
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(1) = True
                    Else
                        BitField(1) = False
                    End If
                    m_nametable_y = value And 1
                End Set
            End Property
            Private m_increment_mode As Byte = 0
            Public Property render_sprites_left() As Byte
                Get
                    Return m_increment_mode
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(2) = True
                    Else
                        BitField(2) = False
                    End If
                    m_increment_mode = value And 1
                End Set
            End Property
            Private m_pattern_sprite As Byte = 0
            Public Property render_background() As Byte
                Get
                    Return m_pattern_sprite
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(3) = True
                    Else
                        BitField(3) = False
                    End If
                    m_pattern_sprite = value And 1
                End Set
            End Property
            Private m_pattern_background As Byte = 0
            Public Property render_sprites() As Byte
                Get
                    Return m_pattern_background
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(4) = True
                    Else
                        BitField(4) = False
                    End If
                    m_pattern_background = value And 1
                End Set
            End Property
            Private m_sprite_size As Byte = 0
            Public Property enhance_red() As Byte
                Get
                    Return m_sprite_size
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(5) = True
                    Else
                        BitField(5) = False
                    End If
                    m_sprite_size = value And 1
                End Set
            End Property
            Private m_slave_mode As Byte = 0
            Public Property enhance_green() As Byte
                Get
                    Return m_slave_mode
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(6) = True
                    Else
                        BitField(6) = False
                    End If
                    m_sprite_size = value And 1
                End Set
            End Property
            Private m_enable_nmi As Byte = 0
            Public Property enhance_blue() As Byte
                Get
                    Return m_enable_nmi
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(7) = True
                    Else
                        BitField(7) = False
                    End If
                    m_enable_nmi = value And 1
                End Set
            End Property
            Public Property reg() As Byte
                Get
                    Dim ReturnValue As Byte = 0
                    For i As Integer = 0 To 7
                        If BitField(i) Then
                            ReturnValue += (1 << i)
                        End If
                    Next
                    Return ReturnValue
                End Get
                Set(value As Byte)
                    For i As Integer = 0 To 7
                        If value And (1 << i) Then
                            BitField(i) = True
                        Else
                            BitField(i) = False
                        End If
                    Next
                    'Set the values
                    m_nametable_x = value And (1 << 0)
                    m_nametable_y = (value And (1 << 1)) >> 1
                    m_increment_mode = (value And (1 << 2)) >> 2
                    m_pattern_sprite = (value And (1 << 3)) >> 3
                    m_pattern_background = (value And (1 << 4)) >> 4
                    m_sprite_size = (value And (1 << 5)) >> 5
                    m_slave_mode = (value And (1 << 6)) >> 6
                    m_enable_nmi = (value And (1 << 7)) >> 7
                End Set
            End Property

        End Class
        'Public Structure FrankenMask
        '    Private m_grayscale As Byte                 ' bit 0
        '    Private m_render_background_left As Byte    ' bit 1
        '    Private m_render_sprites_left As Byte       ' bit 2
        '    Private m_render_background As Byte         ' bit 3
        '    Private m_render_sprites As Byte            ' bit 4

        '    Private m_enhance_red As Byte               ' bit 5
        '    Private m_enhance_green As Byte             ' bit 6
        '    Private m_enhance_blue As Byte              ' bit 7

        '    Private n_TotalByte As Byte
        '    Private Sub Reset()
        '        m_grayscale = 0
        '        m_render_background_left = 0
        '        m_render_sprites_left = 0
        '        m_render_background = 0
        '        m_render_sprites = 0
        '        m_enhance_red = 0
        '        m_enhance_green = 0
        '        m_enhance_blue = 0
        '        n_TotalByte = 0
        '    End Sub

        '    Public Property grayscale() As Byte
        '        Get
        '            Return m_grayscale
        '        End Get
        '        Set(value As Byte)
        '            m_grayscale = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag0) = BitFlag0)
        '            If m_grayscale Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag0
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag0
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property render_background_left() As Byte
        '        Get
        '            Return m_render_background_left
        '        End Get
        '        Set(value As Byte)
        '            m_render_background_left = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag1) = BitFlag1)
        '            If m_render_background_left Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag1
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag1
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property render_sprites_left() As Byte
        '        Get
        '            Return m_render_sprites_left
        '        End Get
        '        Set(value As Byte)
        '            m_render_sprites_left = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag2) = BitFlag2)
        '            If m_render_sprites_left Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag2
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag2
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property render_background() As Byte
        '        Get
        '            Return m_render_background
        '        End Get
        '        Set(value As Byte)
        '            m_render_background = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag3) = BitFlag3)
        '            If m_render_background Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag3
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag3
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property render_sprites() As Byte
        '        Get
        '            Return m_render_sprites
        '        End Get
        '        Set(value As Byte)
        '            m_render_sprites = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag4) = BitFlag4)
        '            If m_render_sprites Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag4
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag4
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property enhance_red() As Byte
        '        Get
        '            Return m_enhance_red
        '        End Get
        '        Set(value As Byte)
        '            m_enhance_red = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag5) = BitFlag5)
        '            If m_enhance_red Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag5
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag5
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property enhance_green() As Byte
        '        Get
        '            Return m_enhance_green
        '        End Get
        '        Set(value As Byte)
        '            m_enhance_green = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag6) = BitFlag6)
        '            If m_enhance_green Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag6
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag6
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property enhance_blue() As Byte
        '        Get
        '            Return m_enhance_blue
        '        End Get
        '        Set(value As Byte)
        '            m_enhance_blue = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag7) = BitFlag7)
        '            If m_enhance_blue Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag7
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag7
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property reg() As Byte
        '        Get
        '            Return n_TotalByte
        '        End Get
        '        Set(value As Byte)
        '            Reset()
        '            If (value And BitFlag0) = BitFlag0 Then n_TotalByte = n_TotalByte Or BitFlag0 : m_grayscale = 1
        '            If (value And BitFlag1) = BitFlag1 Then n_TotalByte = n_TotalByte Or BitFlag1 : m_render_background_left = 1
        '            If (value And BitFlag2) = BitFlag2 Then n_TotalByte = n_TotalByte Or BitFlag2 : m_render_sprites_left = 1
        '            If (value And BitFlag3) = BitFlag3 Then n_TotalByte = n_TotalByte Or BitFlag3 : m_render_background = 1
        '            If (value And BitFlag4) = BitFlag4 Then n_TotalByte = n_TotalByte Or BitFlag4 : m_render_sprites = 1
        '            If (value And BitFlag5) = BitFlag5 Then n_TotalByte = n_TotalByte Or BitFlag5 : m_enhance_red = 1
        '            If (value And BitFlag6) = BitFlag6 Then n_TotalByte = n_TotalByte Or BitFlag6 : m_enhance_green = 1
        '            If (value And BitFlag7) = BitFlag7 Then n_TotalByte = n_TotalByte Or BitFlag7 : m_enhance_blue = 1
        '        End Set
        '    End Property
        'End Structure
        Private PPUMask As New FrankenMask

        Public Class FrankenControl
            Private BitField(7) As Boolean '8-bit union

            Public Sub New()
                For i As Integer = 0 To 7
                    BitField(i) = False
                Next
            End Sub
            Protected Overrides Sub Finalize()
                MyBase.Finalize()
            End Sub


            Private m_nametable_x As Byte = 0
            Public Property nametable_x() As Byte
                Get
                    Return m_nametable_x
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(0) = True
                    Else
                        BitField(0) = False
                    End If
                    m_nametable_x = value And 1
                End Set
            End Property
            Private m_nametable_y As Byte = 0
            Public Property nametable_y() As Byte
                Get
                    Return m_nametable_y
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(1) = True
                    Else
                        BitField(1) = False
                    End If
                    m_nametable_y = value And 1
                End Set
            End Property
            Private m_increment_mode As Byte = 0
            Public Property increment_mode() As Byte
                Get
                    Return m_increment_mode
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(2) = True
                    Else
                        BitField(2) = False
                    End If
                    m_increment_mode = value And 1
                End Set
            End Property
            Private m_pattern_sprite As Byte = 0
            Public Property pattern_sprite() As Byte
                Get
                    Return m_pattern_sprite
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(3) = True
                    Else
                        BitField(3) = False
                    End If
                    m_pattern_sprite = value And 1
                End Set
            End Property
            Private m_pattern_background As Byte = 0
            Public Property pattern_background() As Byte
                Get
                    Return m_pattern_background
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(4) = True
                    Else
                        BitField(4) = False
                    End If
                    m_pattern_background = value And 1
                End Set
            End Property
            Private m_sprite_size As Byte = 0
            Public Property sprite_size() As Byte
                Get
                    Return m_sprite_size
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(5) = True
                    Else
                        BitField(5) = False
                    End If
                    m_sprite_size = value And 1
                End Set
            End Property
            Private m_slave_mode As Byte = 0
            Public Property slave_mode() As Byte
                Get
                    Return m_slave_mode
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(6) = True
                    Else
                        BitField(6) = False
                    End If
                    m_sprite_size = value And 1
                End Set
            End Property
            Private m_enable_nmi As Byte = 0
            Public Property enable_nmi() As Byte
                Get
                    Return m_enable_nmi
                End Get
                Set(value As Byte)
                    If value And &H1 Then
                        BitField(7) = True
                    Else
                        BitField(7) = False
                    End If
                    m_enable_nmi = value And 1
                End Set
            End Property
            Public Property reg() As Byte
                Get
                    Dim ReturnValue As Byte = 0
                    For i As Integer = 0 To 7
                        If BitField(i) Then
                            ReturnValue += (1 << i)
                        End If
                    Next
                    Return ReturnValue
                End Get
                Set(value As Byte)
                    For i As Integer = 0 To 7
                        If value And (1 << i) Then
                            BitField(i) = True
                        Else
                            BitField(i) = False
                        End If
                    Next
                    'Set the values
                    m_nametable_x = value And (1 << 0)
                    m_nametable_y = (value And (1 << 1)) >> 1
                    m_increment_mode = (value And (1 << 2)) >> 2
                    m_pattern_sprite = (value And (1 << 3)) >> 3
                    m_pattern_background = (value And (1 << 4)) >> 4
                    m_sprite_size = (value And (1 << 5)) >> 5
                    m_slave_mode = (value And (1 << 6)) >> 6
                    m_enable_nmi = (value And (1 << 7)) >> 7
                End Set
            End Property

        End Class
        'Public Structure FrankenControl
        '    Private m_nametable_x As Byte           ' bit 0
        '    Private m_nametable_y As Byte           ' bit 1
        '    Private m_increment_mode As Byte        ' bit 2
        '    Private m_pattern_sprite As Byte        ' bit 3
        '    Private m_pattern_background As Byte    ' bit 4

        '    Private m_sprite_size As Byte           ' bit 5
        '    Private m_slave_mode As Byte            ' bit 6
        '    Private m_enable_nmi As Byte            ' bit 7

        '    Private n_TotalByte As Byte
        '    Private Sub Reset()
        '        m_nametable_x = 0
        '        m_nametable_y = 0
        '        m_increment_mode = 0
        '        m_pattern_sprite = 0
        '        m_pattern_background = 0
        '        m_sprite_size = 0
        '        m_slave_mode = 0
        '        m_enable_nmi = 0
        '        n_TotalByte = 0
        '    End Sub

        '    Public Property nametable_x() As Byte
        '        Get
        '            Return m_nametable_x
        '        End Get
        '        Set(value As Byte)
        '            m_nametable_x = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag0) = BitFlag0)
        '            If m_nametable_x Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag0
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag0
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property nametable_y() As Byte
        '        Get
        '            Return m_nametable_y
        '        End Get
        '        Set(value As Byte)
        '            m_nametable_y = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag1) = BitFlag1)
        '            If m_nametable_y Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag1
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag1
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property increment_mode() As Byte
        '        Get
        '            Return m_increment_mode
        '        End Get
        '        Set(value As Byte)
        '            m_increment_mode = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag2) = BitFlag2)
        '            If m_increment_mode Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag2
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag2
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property pattern_sprite() As Byte
        '        Get
        '            Return m_pattern_sprite
        '        End Get
        '        Set(value As Byte)
        '            m_pattern_sprite = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag3) = BitFlag3)
        '            If m_pattern_sprite Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag3
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag3
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property pattern_background() As Byte
        '        Get
        '            Return m_pattern_background
        '        End Get
        '        Set(value As Byte)
        '            m_pattern_background = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag4) = BitFlag4)
        '            If m_pattern_background Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag4
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag4
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property sprite_size() As Byte
        '        Get
        '            Return m_sprite_size
        '        End Get
        '        Set(value As Byte)
        '            m_sprite_size = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag5) = BitFlag5)
        '            If m_sprite_size Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag5
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag5
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property slave_mode() As Byte
        '        Get
        '            Return m_slave_mode
        '        End Get
        '        Set(value As Byte)
        '            m_slave_mode = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag6) = BitFlag6)
        '            If m_slave_mode Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag6
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag6
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property enable_nmi() As Byte
        '        Get
        '            Return m_enable_nmi
        '        End Get
        '        Set(value As Byte)
        '            m_enable_nmi = value And 1
        '            Dim tmp_exists As Boolean = ((n_TotalByte And BitFlag7) = BitFlag7)
        '            If m_enable_nmi Then
        '                If Not tmp_exists Then
        '                    n_TotalByte = n_TotalByte Or BitFlag7
        '                End If
        '            Else
        '                If tmp_exists Then
        '                    n_TotalByte = n_TotalByte Xor BitFlag7
        '                End If
        '            End If
        '        End Set
        '    End Property
        '    Public Property reg() As Byte
        '        Get
        '            Return n_TotalByte
        '        End Get
        '        Set(value As Byte)
        '            Reset()
        '            If (value And BitFlag0) = BitFlag0 Then n_TotalByte = n_TotalByte Or BitFlag0 : m_nametable_x = 1
        '            If (value And BitFlag1) = BitFlag1 Then n_TotalByte = n_TotalByte Or BitFlag1 : m_nametable_y = 1
        '            If (value And BitFlag2) = BitFlag2 Then n_TotalByte = n_TotalByte Or BitFlag2 : m_increment_mode = 1
        '            If (value And BitFlag3) = BitFlag3 Then n_TotalByte = n_TotalByte Or BitFlag3 : m_pattern_sprite = 1
        '            If (value And BitFlag4) = BitFlag4 Then n_TotalByte = n_TotalByte Or BitFlag4 : m_pattern_background = 1
        '            If (value And BitFlag5) = BitFlag5 Then n_TotalByte = n_TotalByte Or BitFlag5 : m_sprite_size = 1
        '            If (value And BitFlag6) = BitFlag6 Then n_TotalByte = n_TotalByte Or BitFlag6 : m_slave_mode = 1
        '            If (value And BitFlag7) = BitFlag7 Then n_TotalByte = n_TotalByte Or BitFlag7 : m_enable_nmi = 1
        '        End Set
        '    End Property
        'End Structure
        Private PPUControl As New FrankenControl

        Public Structure FrankenRegister
            Private m_first5Bits As UInt16
            Private m_second5Bits As UInt16
            Private m_bit0_1 As UInt16
            Private m_bit0_2 As UInt16
            Private m_first3Bits As UInt16
            Private m_bit0_3 As UInt16
            Private m_totalValue As UInt16

            Private Sub Reset()
                m_first5Bits = 0    '5
                m_second5Bits = 0   '5
                m_bit0_1 = 0        '1
                m_bit0_2 = 0        '1
                m_first3Bits = 0    '3
                m_bit0_3 = 0        '1
                m_totalValue = 0    '
            End Sub
            Public Property Coarse_X() As UInt16
                Get
                    Return m_first5Bits
                End Get
                Set(value As UInt16)
                    If m_first5Bits > 0 Then
                        m_totalValue -= m_first5Bits
                    End If
                    m_first5Bits = value And &H1FUS
                    m_totalValue += m_first5Bits '<< 0
                End Set
            End Property
            Public Property Coarse_Y() As UInt16
                Get
                    Return m_second5Bits
                End Get
                Set(value As UInt16)
                    If m_second5Bits > 0 Then
                        m_totalValue -= m_second5Bits << 5
                    End If
                    m_second5Bits = value And &H1FUS
                    m_totalValue += m_second5Bits << 5 '<< [Course_X bit count]
                End Set
            End Property
            Public Property NameTable_X() As UInt16
                Get
                    Return m_bit0_1
                End Get
                Set(value As UInt16)
                    If m_bit0_1 > 0 Then
                        m_totalValue -= (m_bit0_1 << 10)
                    End If
                    m_bit0_1 = value And &H1US
                    m_totalValue += m_bit0_1 << 10 '<< [Course_X + Course_Y bit count]
                End Set
            End Property
            Public Property NameTable_Y() As UInt16
                Get
                    Return m_bit0_2
                End Get
                Set(value As UInt16)
                    If m_bit0_2 > 0 Then
                        m_totalValue -= (m_bit0_2 << 11)
                    End If
                    m_bit0_2 = value And &H1US
                    m_totalValue += (m_bit0_2 << 11) '<< [Course_X + Course_Y + NameTable_X bit count]
                End Set
            End Property
            Public Property Fine_Y() As UInt16
                Get
                    Return m_first3Bits
                End Get
                Set(value As UInt16)
                    If m_first3Bits > 0 Then
                        m_totalValue -= (m_first3Bits << 12)
                    End If
                    m_first3Bits = value And &H7US
                    m_totalValue += (m_first3Bits << 12) '<< [Course_X + Course_Y + NameTable_X + NameTable_Y bit count]
                End Set
            End Property
            Public Property Unused() As UInt16
                Get
                    Return m_bit0_3
                End Get
                Set(value As UInt16)
                    If m_bit0_3 > 0 Then
                        m_totalValue -= (m_bit0_3 << 15)
                    End If
                    m_bit0_3 = value And &H8000US
                    m_totalValue += (m_bit0_3 << 15) '<< [Course_X + Course_Y + NameTable_X + NameTable_Y + Fine_Y bit count]
                End Set
            End Property
            Public Property Reg() As UInt16
                Get
                    Return m_totalValue '(m_first5Bits + m_second5Bits + m_bit0_1 + m_bit0_2 + m_first3Bits + m_bit0_3)
                    'Return m_totalValue
                End Get
                Set(value As UInt16)
                    'Reset our totals
                    Reset()                                     'I Need more info
                    If value > 0 Then                               'FEDCBA9876543210
                        m_first5Bits = (value And &H1FUS) '>> 0     '0000000000011111 << 11 ' 0
                        m_second5Bits = (value And &H3E0US) >> 5    '0000001111100000 << 6  ' 5
                        m_bit0_1 = (value And &H400US) >> 10        '0000010000000000 << 5  '10
                        m_bit0_2 = (value And &H800US) >> 11        '0000100000000000 << 4  '11
                        m_first3Bits = (value And &H7000US) >> 12   '0111000000000000 << 1  '12
                        m_bit0_3 = (value And &H8000US) >> 15       '1000000000000000 << 0  '15
                        m_totalValue = value '(m_first5Bits + m_second5Bits + m_bit0_1 + m_bit0_2 + m_first3Bits + m_bit0_3)                        ' Total combined value
                    End If
                End Set
            End Property
        End Structure

        Public Class FrankenLoopy
            Private BitFeild(15) As Boolean '16-bit union
            'm_first5Bits = 0    '5
            'm_second5Bits = 0   '5
            'm_bit0_1 = 0        '1
            'm_bit0_2 = 0        '1
            'm_first3Bits = 0    '3
            'm_bit0_3 = 0        '1
            'm_totalValue = 0    '

            Public Sub New()
                For i As Integer = 0 To BitFeild.Length() - 1
                    BitFeild(i) = False
                Next
            End Sub

            Private m_cx As UInt16 = 0
            Public Property Coarse_X() As UInt16
                Get
                    Return m_cx
                End Get
                Set(value As UInt16)
                    For i As Integer = 0 To 4 '5
                        If value And (1 << i) Then
                            BitFeild(i) = True
                        Else
                            BitFeild(i) = False
                        End If
                    Next
                    m_cx = (value And &H1FUS)
                End Set
            End Property
            Private m_cy As UInt16 = 0
            Public Property Coarse_Y() As UInt16
                Get
                    Return m_cy
                End Get
                Set(value As UInt16)
                    For i As Integer = 0 To 4 '5
                        If value And (1 << i) Then
                            BitFeild(5 + i) = True
                        Else
                            BitFeild(5 + i) = False
                        End If
                    Next
                    m_cy = (value And &H1FUS)
                End Set
            End Property
            Private m_nt_x As UInt16 = 0
            Public Property NameTable_X() As UInt16
                Get
                    Return m_nt_x
                End Get
                Set(value As UInt16)
                    If value And (1 << 0) Then
                        BitFeild(10) = True
                    Else
                        BitFeild(10) = False
                    End If
                    m_nt_x = (value And &H1US)
                End Set
            End Property
            Private m_nt_y As UInt16 = 0
            Public Property NameTable_Y() As UInt16
                Get
                    Return m_nt_y
                End Get
                Set(value As UInt16)
                    If value And (1 << 0) Then
                        BitFeild(11) = True
                    Else
                        BitFeild(11) = False
                    End If
                    m_nt_y = (value And &H1US)
                End Set
            End Property
            Private m_f_y As UInt16 = 0
            Public Property Fine_Y() As UInt16
                Get
                    Return m_f_y
                End Get
                Set(value As UInt16)
                    For i As Integer = 0 To 2 '3
                        If value And (1 << i) Then
                            BitFeild(12 + i) = True
                        Else
                            BitFeild(12 + i) = False
                        End If
                    Next
                    m_f_y = (value And &H7US)
                End Set
            End Property
            Private m_unused As UInt16 = 0
            Public Property unused() As UInt16
                Get
                    Return m_unused
                End Get
                Set(value As UInt16)
                    If value And (1 << 0) Then
                        BitFeild(15) = True
                    Else
                        BitFeild(15) = False
                    End If
                    m_unused = (value And &H1US)
                End Set
            End Property
            Public Property Reg() As UInt16
                Get
                    Dim ReturnValue As UInt16 = 0
                    For i As Integer = 0 To 15
                        If BitFeild(i) Then
                            ReturnValue += (1 << i)
                        End If
                    Next
                    Return ReturnValue
                End Get
                Set(value As UInt16)
                    For i As Integer = 0 To 15
                        If value And (1 << i) Then
                            BitFeild(i) = True
                        Else
                            BitFeild(i) = False
                        End If
                    Next
                    'Set the values
                    m_cx = (value And &H1FUS)
                    m_cy = (value And &H3E0US) >> 5
                    m_nt_x = (value And &H400US) >> 10
                    m_nt_y = (value And &H800US) >> 11
                    m_f_y = (value And &H7000US) >> 12
                    m_unused = (value And &H8000US) >> 15
                End Set
            End Property

        End Class

        'some how the franken register gives a sort of screen lol
        Public Shared vram_addr As New FrankenLoopy
        Private Shared tram_addr As New FrankenLoopy

        Public Sub New()
            sprNameTable = {New GraphicsObjects.Sprite(256, 240), New GraphicsObjects.Sprite(256, 240)}
            sprPatternTable = {New GraphicsObjects.Sprite(128, 128), New GraphicsObjects.Sprite(128, 128)}

            palScreen(&H0) = New GraphicsObjects.Pixel(84, 84, 84)
            palScreen(&H1) = New GraphicsObjects.Pixel(0, 30, 116)
            palScreen(&H2) = New GraphicsObjects.Pixel(8, 16, 144)
            palScreen(&H3) = New GraphicsObjects.Pixel(48, 0, 136)
            palScreen(&H4) = New GraphicsObjects.Pixel(68, 0, 100)
            palScreen(&H5) = New GraphicsObjects.Pixel(92, 0, 48)
            palScreen(&H6) = New GraphicsObjects.Pixel(84, 4, 0)
            palScreen(&H7) = New GraphicsObjects.Pixel(60, 24, 0)
            palScreen(&H8) = New GraphicsObjects.Pixel(32, 42, 0)
            palScreen(&H9) = New GraphicsObjects.Pixel(8, 58, 0)
            palScreen(&HA) = New GraphicsObjects.Pixel(0, 64, 0)
            palScreen(&HB) = New GraphicsObjects.Pixel(0, 60, 0)
            palScreen(&HC) = New GraphicsObjects.Pixel(0, 50, 60)
            palScreen(&HD) = New GraphicsObjects.Pixel(0, 0, 0)
            palScreen(&HE) = New GraphicsObjects.Pixel(0, 0, 0)
            palScreen(&HF) = New GraphicsObjects.Pixel(0, 0, 0)

            palScreen(&H10) = New GraphicsObjects.Pixel(152, 150, 152)
            palScreen(&H11) = New GraphicsObjects.Pixel(8, 76, 196)
            palScreen(&H12) = New GraphicsObjects.Pixel(48, 50, 236)
            palScreen(&H13) = New GraphicsObjects.Pixel(92, 30, 228)
            palScreen(&H14) = New GraphicsObjects.Pixel(136, 20, 176)
            palScreen(&H15) = New GraphicsObjects.Pixel(160, 20, 100)
            palScreen(&H16) = New GraphicsObjects.Pixel(152, 34, 32)
            palScreen(&H17) = New GraphicsObjects.Pixel(120, 60, 0)
            palScreen(&H18) = New GraphicsObjects.Pixel(84, 90, 0)
            palScreen(&H19) = New GraphicsObjects.Pixel(40, 114, 0)
            palScreen(&H1A) = New GraphicsObjects.Pixel(8, 124, 0)
            palScreen(&H1B) = New GraphicsObjects.Pixel(0, 118, 40)
            palScreen(&H1C) = New GraphicsObjects.Pixel(0, 102, 120)
            palScreen(&H1D) = New GraphicsObjects.Pixel(0, 0, 0)
            palScreen(&H1E) = New GraphicsObjects.Pixel(0, 0, 0)
            palScreen(&H1F) = New GraphicsObjects.Pixel(0, 0, 0)

            palScreen(&H20) = New GraphicsObjects.Pixel(236, 238, 236)
            palScreen(&H21) = New GraphicsObjects.Pixel(76, 154, 236)
            palScreen(&H22) = New GraphicsObjects.Pixel(120, 124, 236)
            palScreen(&H23) = New GraphicsObjects.Pixel(176, 98, 236)
            palScreen(&H24) = New GraphicsObjects.Pixel(228, 84, 236)
            palScreen(&H25) = New GraphicsObjects.Pixel(236, 88, 180)
            palScreen(&H26) = New GraphicsObjects.Pixel(236, 106, 100)
            palScreen(&H27) = New GraphicsObjects.Pixel(212, 136, 32)
            palScreen(&H28) = New GraphicsObjects.Pixel(160, 170, 0)
            palScreen(&H29) = New GraphicsObjects.Pixel(116, 196, 0)
            palScreen(&H2A) = New GraphicsObjects.Pixel(76, 208, 32)
            palScreen(&H2B) = New GraphicsObjects.Pixel(56, 204, 108)
            palScreen(&H2C) = New GraphicsObjects.Pixel(56, 180, 204)
            palScreen(&H2D) = New GraphicsObjects.Pixel(60, 60, 60)
            palScreen(&H2E) = New GraphicsObjects.Pixel(0, 0, 0)
            palScreen(&H2F) = New GraphicsObjects.Pixel(0, 0, 0)

            palScreen(&H30) = New GraphicsObjects.Pixel(236, 238, 236)
            palScreen(&H31) = New GraphicsObjects.Pixel(168, 204, 236)
            palScreen(&H32) = New GraphicsObjects.Pixel(188, 188, 236)
            palScreen(&H33) = New GraphicsObjects.Pixel(212, 178, 236)
            palScreen(&H34) = New GraphicsObjects.Pixel(236, 174, 236)
            palScreen(&H35) = New GraphicsObjects.Pixel(236, 174, 212)
            palScreen(&H36) = New GraphicsObjects.Pixel(236, 180, 176)
            palScreen(&H37) = New GraphicsObjects.Pixel(228, 196, 144)
            palScreen(&H38) = New GraphicsObjects.Pixel(204, 210, 120)
            palScreen(&H39) = New GraphicsObjects.Pixel(180, 222, 120)
            palScreen(&H3A) = New GraphicsObjects.Pixel(168, 226, 144)
            palScreen(&H3B) = New GraphicsObjects.Pixel(152, 226, 180)
            palScreen(&H3C) = New GraphicsObjects.Pixel(160, 214, 228)
            palScreen(&H3D) = New GraphicsObjects.Pixel(160, 162, 160)
            palScreen(&H3E) = New GraphicsObjects.Pixel(0, 0, 0)
            palScreen(&H3F) = New GraphicsObjects.Pixel(0, 0, 0)
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub

        Public Function GetScreen() As GraphicsObjects.Sprite
            Return sprScreen
        End Function
        Public Function GetNameTable(ByVal index As Byte) As GraphicsObjects.Sprite
            Return sprNameTable(index)
        End Function
        Public Function GetPatternTable(ByVal i As Byte, ByVal palette As Byte) As GraphicsObjects.Sprite
            '// This function draw the CHR ROM for a given pattern table into
            '// an olc:Sprite, using a specified palette. Pattern tables consist
            '// of 16x16 "tiles or characters". It Is independent of the running
            '// emulation And using it does Not change the systems state, though
            '// it gets all the data it needs from the live system. Consequently,
            '// if the game has Not yet established palettes Or mapped to relevant
            '// CHR ROM banks, the sprite may look empty. This approach permits a 
            '// "live" extraction of the pattern table exactly how the NES, And 
            '// ultimately the player would see it.

            '// A tile consists of 8x8 pixels. On the NES, pixels are 2 bits, which
            '// gives an index into 4 different colours of a specific palette. There
            '// are 8 palettes to choose from. Colour "0" in each palette Is effectively
            '// considered transparent, as those locations in memory "mirror" the global
            '// background colour being used. This mechanics of this are shown in 
            '// detail in ppuRead() & ppuWrite()

            '// Characters on NES
            '// ~~~~~~~~~~~~~~~~~
            '// The NES stores characters using 2-bit pixels. These are Not stored sequentially
            '// but in singular bit planes. For example:
            '//
            '	// 2-Bit Pixels       LSB Bit Plane     MSB Bit Plane
            '// 0 0 0 0 0 0 0 0	  0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0
            '// 0 1 1 0 0 1 1 0	  0 1 1 0 0 1 1 0   0 0 0 0 0 0 0 0
            '// 0 1 2 0 0 2 1 0	  0 1 1 0 0 1 1 0   0 0 1 0 0 1 0 0
            '// 0 0 0 0 0 0 0 0 =  0 0 0 0 0 0 0 0 + 0 0 0 0 0 0 0 0
            '// 0 1 1 0 0 1 1 0	  0 1 1 0 0 1 1 0   0 0 0 0 0 0 0 0
            '// 0 0 1 1 1 1 0 0	  0 0 1 1 1 1 0 0   0 0 0 0 0 0 0 0
            '// 0 0 0 2 2 0 0 0	  0 0 0 1 1 0 0 0   0 0 0 1 1 0 0 0
            '// 0 0 0 0 0 0 0 0	  0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0
            '//
            '// The planes are stored as 8 bytes of LSB, followed by 8 bytes of MSB

            '// Loop through all 16x16 tiles
            For nTileY As UInt16 = 0 To 15 'VB
                For nTileX As UInt16 = 0 To 15 'VB
                    '// Convert the 2D tile coordinate into a 1D offset into the pattern
                    '// table memory.
                    'uint16_t nOffset = nTileY * 256 + nTileX * 16;
                    Dim nOffset As UInt16 = MathHelpers.SafeAddition16(MathHelpers.SafeMul16(nTileY, 256), MathHelpers.SafeMul16(nTileX, 16))

                    '// Now loop through 8 rows of 8 pixels
                    For row As UInt16 = 0 To 7 'VB
                        '// For each row, we need to read both bit planes of the character
                        '// in order to extract the least significant And most significant 
                        '// bits of the 2 bit pixel value. in the CHR ROM, each character
                        '// Is stored as 64 bits of lsb, followed by 64 bits of msb. This
                        '// conveniently means that two corresponding rows are always 8
                        '// bytes apart in memory.
                        Dim tile_lsb As Byte = ppuRead(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(MathHelpers.SafeMul16(i, &H1000US), nOffset), row), &H0US))
                        Dim tile_msb As Byte = ppuRead(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(MathHelpers.SafeMul16(i, &H1000US), nOffset), row), &H8US))


                        '// Now we have a single row of the two bit planes for the character
                        '// we need to iterate through the 8-bit words, combining them to give
                        '// us the final pixel index
                        For col As UInt16 = 0 To 7 'VB
                            '// We can get the index value by simply adding the bits together
                            '// but we're only interested in the lsb of the row words because...
                            Dim pixel As Byte = MathHelpers.SafeShiftLeft8((tile_msb And &H1UI), 1) Or (tile_lsb And &H1UI)

                            '// ...we will shift the row words 1 bit right for each column of
                            '// the character.
                            tile_lsb = MathHelpers.SafeShiftRight8(tile_lsb, 1) ' >>= 1
                            tile_msb = MathHelpers.SafeShiftRight8(tile_msb, 1) ' >>= 1

                            '// Now we know the location And NES pixel value for a specific location
                            '// in the pattern table, we can translate that to a screen colour, And an
                            '// (x,y) location in the sprite

                            'nTileX * 8 + (7 - col), // Because we are using the lsb of the row word first
                            '// we are effectively reading the row from right
                            '// to left, so we need to draw the row "backwards"					    
                            sprPatternTable(i).SetPixel(MathHelpers.SafeAddition16(MathHelpers.SafeMul16(nTileX, 8), MathHelpers.SafeSubtract16(7, col)),
                                                        MathHelpers.SafeAddition16(MathHelpers.SafeMul16(nTileY, 8), row),
                                                        GetColorFromPaletteRam(palette, pixel))
                            'sprPatternTable(i).SetPixel(nTileX * 8 + (7 - col),
                            '                            nTileY * 8 + row,
                            '                            GetColorFromPaletteRam(palette, pixel))
                        Next
                    Next
                Next
            Next
            '// Finally return the updated sprite representing the pattern table
            Return sprPatternTable(i)
        End Function
        Public Function GetColorFromPaletteRam(ByVal arPalette As Byte, ByVal arPixel As Byte) As GraphicsObjects.Pixel
            '// This Is a convenience function that takes a specified palette And pixel
            '// index And returns the appropriate screen colour.
            '// "0x3F00"       - Offset into PPU addressable range where palettes are stored
            '// "palette << 2" - Each palette Is 4 bytes in size
            '// "pixel"        - Each pixel index Is either 0, 1, 2 Or 3
            '// "& 0x3F"       - Stops us reading beyond the bounds of the palScreen array
            Return palScreen(ppuRead(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(&H3F00US, MathHelpers.SafeShiftLeft16(arPalette, 2)), arPixel)) And &H3FUS)

            '// Note We dont access tblPalette directly here, instead we know that ppuRead()
            '// will map the address onto the seperate small RAM attached to the PPU bus.
        End Function

        ' Communications with the main bus
        Public Function cpuRead(ByVal addr As UInt16, ByVal Optional rdOnly As Boolean = False) As Byte
            Dim bytData As Byte = 0
            If rdOnly Then
                '// Reading from PPU registers can affect their contents
                '// so this read only option Is used for examining the
                '// state of the PPU without changing its state. This Is
                '// really only used in debug mode.
                Select Case addr
                    Case &H0US : bytData = PPUControl.reg : Exit Select ' Control
                    Case &H1US : bytData = PPUMask.reg : Exit Select    ' Mask
                    Case &H2US : bytData = PPUStatus.reg : Exit Select  ' Status
                    Case &H3US : Exit Select                         ' OAM Address
                    Case &H4US : Exit Select                         ' OAM Data
                    Case &H5US : Exit Select                         ' Scroll
                    Case &H6US : Exit Select                         ' PPU Address
                    Case &H7US : Exit Select                         ' PPU Data
                End Select
            Else
                '// These are the live PPU registers that repsond
                '// to being read from in various ways. Note that Not
                '// all the registers are capable of being read from
                '// so they just return 0x00
                Select Case addr
                    Case &H0US : Exit Select ' Control - Not readable
                    Case &H1US : Exit Select ' Mask - Not Readable
                    Case &H2US               ' Status
                        '// Reading from the status register has the effect of resetting
                        '// different parts of the circuit. Only the top three bits
                        '// contain status information, however it Is possible that
                        '// some "noise" gets picked up on the bottom 5 bits which 
                        '// represent the last PPU bus transaction. Some games "may"
                        '// use this noise as valid data (even though they probably
                        '// shouldn't)
                        bytData = (PPUStatus.reg And &HE0UI) Or (ppu_data_buffer And &H1FUI)
                        '// Clear the vertical blanking flag
                        PPUStatus.vertical_blank = 0
                        '// Reset Loopy's Address latch flag
                        address_latch = 0
                        Exit Select
                    Case &H3US : Exit Select                            ' OAM Address 
                    Case &H4US
                        bytData = OAM(oam_addr \ 4).GetByteAt(oam_addr) ' [\ 4 = 0->63][Mod 4 = 0->3] ' will the oam_addr ever be 256 or +? 
                        ' TODO: ReDesign OAM [OAM(
                        Exit Select                         ' OAM Data
                    Case &H5US : Exit Select                         ' Scroll
                    Case &H6US : Exit Select                         ' PPU Address
                    Case &H7US                                       ' PPU Data
                        '// Reads from the NameTable ram get delayed one cycle, 
                        '// so output buffer which contains the data from the 
                        '// previous read request
                        bytData = ppu_data_buffer
                        '// then update the buffer for next time
                        ppu_data_buffer = ppuRead(vram_addr.Reg)
                        '// However, if the address was in the palette range, the
                        '// data Is Not delayed, so it returns immediately
                        If (vram_addr.Reg >= &H3F00US) Then bytData = ppu_data_buffer
                        '// All reads from PPU data automatically increment the nametable
                        '// address depending upon the mode set in the control register.
                        '// If set to vertical mode, the increment Is 32, so it skips
                        '// one whole nametable row; in horizontal mode it just increments
                        '// by 1, moving to the next column
                        If PPUControl.increment_mode Then
                            vram_addr.Reg = vram_addr.Reg + 32
                        Else
                            vram_addr.Reg = vram_addr.Reg + 1
                        End If
                        Exit Select
                End Select
            End If
            Return bytData
        End Function
        Public Sub cpuWrite(ByVal addr As UInt16, ByVal data As Byte)
            Select Case addr
                Case &H0US
                    PPUControl.reg = data
                    tram_addr.NameTable_X = PPUControl.nametable_x
                    tram_addr.NameTable_Y = PPUControl.nametable_y
                    Exit Select
                Case &H1US
                    PPUMask.reg = data
                    Exit Select
                Case &H2US
                    Exit Select
                Case &H3US
                    oam_addr = data
                    Exit Select
                Case &H4US
                    OAM(oam_addr \ 4).SetByteAt(oam_addr, data)
                    Exit Select
                Case &H5US
                    If address_latch = 0 Then
                        fine_x = data And &H7UI
                        tram_addr.Coarse_X = data >> 3
                        address_latch = 1
                    Else
                        tram_addr.Fine_Y = data And &H7UI
                        tram_addr.Coarse_Y = data >> 3
                        address_latch = 0
                    End If
                    Exit Select
                Case &H6US
                    If address_latch = 0 Then
                        tram_addr.Reg = ((data And &H3FUS) << 8) Or (tram_addr.Reg And &HFFUS)
                        address_latch = 1
                    Else
                        tram_addr.Reg = (tram_addr.Reg And &HFF00US) Or data
                        vram_addr = tram_addr
                        address_latch = 0
                    End If
                    Exit Select
                Case &H7US
                    ppuWrite(vram_addr.Reg, data)
                    If PPUControl.increment_mode Then
                        vram_addr.Reg = vram_addr.Reg + 31
                    Else
                        vram_addr.Reg = vram_addr.Reg + 1
                    End If
                    Exit Select
            End Select
        End Sub
        ' Communications with the PPU bus
        Public Function ppuRead(ByVal addr1 As UInt16, ByVal Optional rdOnly As Boolean = False) As Byte
            Dim data As Byte = &H0UI
            Dim addr As UInt16 = addr1 And &H3FFFUS
            If Cart.ppuRead(addr, data) Then
                ' Was Cartridge Read 
            ElseIf addr >= &H0US AndAlso addr <= &H1FFFUS Then
                data = tblPattern((addr And &H1000US) >> 12, addr And &HFFFUS)
            ElseIf addr >= &H2000US AndAlso addr <= &H3EFFUS Then
                addr = addr And &HFFFUS
                If Cart.Mirror() = clsMapper.enMIRROR.VERTICAL Then
                    If addr >= &H0US AndAlso addr <= &H3FFUS Then
                        data = tblName(0, addr And &H3FFUS)
                    End If
                    If addr >= &H400US AndAlso addr <= &H7FFUS Then
                        data = tblName(1, addr And &H3FFUS)
                    End If
                    If addr >= &H800US AndAlso addr <= &HBFFUS Then
                        data = tblName(0, addr And &H3FFUS)
                    End If
                    If addr >= &HC00US AndAlso addr <= &HFFFUS Then
                        data = tblName(1, addr And &H3FFUS)
                    End If
                ElseIf Cart.Mirror() = clsMapper.enMIRROR.HORIZONTAL Then
                    If addr >= &H0US AndAlso addr <= &H3FFUS Then
                        data = tblName(0, addr And &H3FFUS)
                    End If
                    If addr >= &H400US AndAlso addr <= &H7FFUS Then
                        data = tblName(0, addr And &H3FFUS)
                    End If
                    If addr >= &H800US AndAlso addr <= &HBFFUS Then
                        data = tblName(1, addr And &H3FFUS)
                    End If
                    If addr >= &HC00US AndAlso addr <= &HFFFUS Then
                        data = tblName(1, addr And &H3FFUS)
                    End If
                End If
            ElseIf addr >= &H3F00US AndAlso addr <= &H3FFFUS Then
                addr = addr And &H1FUS
                Select Case addr
                    Case &H10US
                        addr = &H0US
                        Exit Select
                    Case &H14US
                        addr = &H4US
                        Exit Select
                    Case &H18US
                        addr = &H8US
                        Exit Select
                    Case &H1CUS
                        addr = &HCUS
                        Exit Select
                End Select
                If PPUMask.grayscale Then
                    data = tblPalette(addr) And &H30UI
                Else
                    data = tblPalette(addr) And &H3FUI
                End If
            End If
            Return data
        End Function
        Public Sub ppuWrite(ByVal addr As UInt16, ByVal data As Byte)
            addr = addr And &H3FFFUS
            If Cart.ppuWrite(addr, data) Then
                ' Was Cartridge Write
            ElseIf addr >= &H0US AndAlso addr <= &H1FFFUS Then
                tblPattern((addr And &H1000US) >> 12, addr And &HFFFUS) = data
            ElseIf addr >= &H2000US AndAlso addr <= &H3EFFUS Then
                addr = addr And &HFFFUS
                If Cart.Mirror() = clsMapper.enMIRROR.VERTICAL Then
                    If addr >= &H0US AndAlso addr <= &H3FFUS Then
                        tblName(0, addr And &H3FFUS) = data
                    End If
                    If addr >= &H400US AndAlso addr <= &H7FFUS Then
                        tblName(1, addr And &H3FFUS) = data
                    End If
                    If addr >= &H800US AndAlso addr <= &HBFFUS Then
                        tblName(0, addr And &H3FFUS) = data
                    End If
                    If addr >= &HC00US AndAlso addr <= &HFFFUS Then
                        tblName(1, addr And &H3FFUS) = data
                    End If
                ElseIf Cart.Mirror() = clsMapper.enMIRROR.HORIZONTAL Then
                    If addr >= &H0US AndAlso addr <= &H3FFUS Then
                        tblName(0, addr And &H3FFUS) = data
                    End If
                    If addr >= &H400US AndAlso addr <= &H7FFUS Then
                        tblName(0, addr And &H3FFUS) = data
                    End If
                    If addr >= &H800US AndAlso addr <= &HBFFUS Then
                        tblName(1, addr And &H3FFUS) = data
                    End If
                    If addr >= &HC00US AndAlso addr <= &HFFFUS Then
                        tblName(1, addr And &H3FFUS) = data
                    End If
                End If
            ElseIf addr >= &H3F00US AndAlso addr <= &H3FFFUS Then
                addr = addr And &H1FUS
                Select Case addr
                    Case &H10US
                        addr = &H0US
                        Exit Select
                    Case &H14US
                        addr = &H4US
                        Exit Select
                    Case &H18US
                        addr = &H8US
                        Exit Select
                    Case &H1CUS
                        addr = &HCUS
                        Exit Select
                End Select
                tblPalette(addr) = data
            End If
        End Sub

        'Public Cart As clsCartridge
        'Public Sub ConnectCartridge(ByRef cartridge As clsCartridge)
        '    Cart = cartridge
        'End Sub
        'Public Sub RemoveCartridge()
        '    Cart = Nothing
        'End Sub

        Public Sub Reset()
            fine_x = 0
            address_latch = 0
            ppu_data_buffer = 0
            scanline = 0
            cycle = 0
            bg_next_tile_id = 0
            bg_next_tile_attrib = 0
            bg_next_tile_lsb = 0
            bg_next_tile_msb = 0
            bg_shifter_pattern_lo = 0
            bg_shifter_pattern_hi = 0
            bg_shifter_attrib_lo = 0
            bg_shifter_attrib_hi = 0
            PPUStatus.reg = 0
            PPUMask.reg = 0
            PPUControl.reg = 0
            vram_addr.Reg = 0
            tram_addr.Reg = 0
            scanline_trigger = False
            odd_frame = False
            Return
        End Sub

        Public Sub Clock()

            Dim IncrementScrollX = Sub()
                                       If PPUMask.render_background OrElse PPUMask.render_sprites Then
                                           If vram_addr.Coarse_X = 31 Then
                                               vram_addr.Coarse_X = 0
                                               vram_addr.NameTable_X = Not vram_addr.NameTable_X 'VBMATH
                                           Else
                                               vram_addr.Coarse_X = vram_addr.Coarse_X + 1
                                           End If
                                       End If
                                   End Sub
            Dim IncrementScrollY = Sub()
                                       If PPUMask.render_background OrElse PPUMask.render_sprites Then
                                           If vram_addr.Fine_Y < 7 Then
                                               vram_addr.Fine_Y = vram_addr.Fine_Y + 1
                                           Else
                                               vram_addr.Fine_Y = 0

                                               If vram_addr.Coarse_Y = 29 Then
                                                   vram_addr.Coarse_Y = 0
                                                   vram_addr.NameTable_Y = Not vram_addr.NameTable_Y
                                               ElseIf vram_addr.Coarse_Y = 31 Then
                                                   vram_addr.Coarse_Y = 0
                                               Else
                                                   vram_addr.Coarse_Y = vram_addr.Coarse_Y + 1
                                               End If
                                           End If
                                       End If
                                   End Sub
            Dim TransferAddressX = Sub()
                                       If PPUMask.render_background OrElse PPUMask.render_sprites Then
                                           vram_addr.NameTable_X = tram_addr.NameTable_X
                                           vram_addr.Coarse_X = tram_addr.Coarse_X
                                       End If
                                   End Sub
            Dim TransferAddressY = Sub()
                                       If PPUMask.render_background OrElse PPUMask.render_sprites Then
                                           vram_addr.Fine_Y = tram_addr.Fine_Y
                                           vram_addr.NameTable_Y = tram_addr.NameTable_Y
                                           vram_addr.Coarse_Y = tram_addr.Coarse_Y
                                       End If
                                   End Sub
            Dim LoadBackgroundShifters = Sub()
                                             bg_shifter_pattern_lo = (bg_shifter_pattern_lo And &HFF00US) Or bg_next_tile_lsb
                                             bg_shifter_pattern_hi = (bg_shifter_pattern_hi And &HFF00US) Or bg_next_tile_msb
                                             bg_shifter_attrib_lo = (bg_shifter_attrib_lo And &HFF00US) Or IIf(bg_next_tile_attrib And &H1, &HFFUI, &H0UI)
                                             bg_shifter_attrib_hi = (bg_shifter_attrib_hi And &HFF00US) Or IIf(bg_next_tile_attrib And &H2, &HFFUI, &H0UI)
                                         End Sub
            Dim UpdateShifters = Sub()
                                     If PPUMask.render_background Then
                                         bg_shifter_pattern_lo <<= 1
                                         bg_shifter_pattern_hi <<= 1

                                         bg_shifter_attrib_lo <<= 1
                                         bg_shifter_attrib_hi <<= 1
                                     End If
                                     If PPUMask.render_sprites AndAlso cycle >= 1 AndAlso cycle < 258 Then
                                         If sprite_count > 0 Then
                                             For i As UInt32 = 0 To (sprite_count - 1)
                                                 If spriteScanline(i).x > 0 Then
                                                     spriteScanline(i).x = spriteScanline(i).x - 1
                                                 Else
                                                     sprite_shifter_pattern_lo(i) <<= 1
                                                     sprite_shifter_pattern_hi(i) <<= 1
                                                 End If
                                             Next
                                         End If
                                     End If
                                 End Sub

            'Dim thisx As Integer 'Was Debuging
            'If ClockCounter = 3068 Then
            '    thisx = 1
            'Else
            '    thisx = 2
            'End If

            If scanline >= -1 AndAlso scanline < 240 Then
                ' Background Rendering
                If scanline = 0 AndAlso cycle = 0 AndAlso odd_frame AndAlso (PPUMask.render_background OrElse PPUMask.render_sprites) Then
                    ' Odd frame, cycle skip
                    cycle = 1
                End If
                '-----------------------------
                If scanline = -1 AndAlso cycle = 1 Then
                    '// Effectively start of new frame, so clear vertical blank flag
                    PPUStatus.vertical_blank = 0
                    '// Clear sprite overflow flag
                    PPUStatus.sprite_overflow = 0
                    '// Clear the sprite zero hit flag
                    PPUStatus.sprite_zero_hit = 0
                    '// Clear Shifters
                    For i As UInt32 = 0 To 7 'VB
                        sprite_shifter_pattern_lo(i) = 0
                        sprite_shifter_pattern_hi(i) = 0
                    Next
                End If
                '-----------------------------
                If (cycle >= 2 AndAlso cycle < 258) OrElse (cycle >= 321 AndAlso cycle < 338) Then
                    UpdateShifters()

                    '// In these cycles we are collecting And working with visible data
                    '// The "shifters" have been preloaded by the end of the previous
                    '// scanline with the data for the start of this scanline. Once we
                    '// leave the visible region, we go dormant until the shifters are
                    '// preloaded for the next scanline.

                    '// Fortunately, for background rendering, we go through a fairly
                    '// repeatable sequence of events, every 2 clock cycles.
                    Select Case ((cycle - 1) Mod 8)
                        Case 0
                            '// Load the current background tile pattern and attributes into the "shifter"
                            LoadBackgroundShifters()
                            '// Fetch the next background tile ID
                            '// "(vram_addr.reg & 0x0FFF)" : Mask to 12 bits that are relevant
                            '// "| 0x2000"                 : Offset into nametable space on PPU address bus
                            bg_next_tile_id = ppuRead(&H2000US Or (vram_addr.Reg And &HFFFUS))

                            '// Explanation
                            '// The bottom 12 bits of the loopy register provide an index into
                            '// the 4 nametables, regardless of nametable mirroring configuration.
                            '// nametable_y(1) nametable_x(1) coarse_y(5) coarse_x(5)
                            '//
                            '// Consider a single nametable Is a 32x32 array, And we have four of them
                            '//   0                1
                            '// 0 +----------------+----------------+
                            '//   |                |                |
                            '//   |                |                |
                            '//   |    (32x32)     |    (32x32)     |
                            '//   |                |                |
                            '//   |                |                |
                            '// 1 +----------------+----------------+
                            '//   |                |                |
                            '//   |                |                |
                            '//   |    (32x32)     |    (32x32)     |
                            '//   |                |                |
                            '//   |                |                |
                            '//   +----------------+----------------+
                            '//
                            '// This means there are 4096 potential locations in this array, which 
                            '// just so happens to be 2^12!
                            Exit Select
                        Case 2
                            '// Fetch the next background tile attribute. OK, so this one Is a bit
                            '// more involved :P

                            '// Recall that each nametable has two rows of cells that are Not tile 
                            '// information, instead they represent the attribute information that
                            '// indicates which palettes are applied to which area on the screen.
                            '// Importantly (And frustratingly) there Is Not a 1 to 1 correspondance
                            '// between background tile And palette. Two rows of tile data holds
                            '// 64 attributes. Therfore we can assume that the attributes affect
                            '// 8x8 zones on the screen for that nametable. Given a working resolution
                            '// of 256x240, we can further assume that each zone Is 32x32 pixels
                            '// in screen space, Or 4x4 tiles. Four system palettes are allocated
                            '// to background rendering, so a palette can be specified using just
                            '// 2 bits. The attribute byte therefore can specify 4 distinct palettes.
                            '// Therefore we can even further assume that a single palette Is
                            '// applied to a 2x2 tile combination of the 4x4 tile zone. The very fact
                            '// that background tiles "share" a palette locally Is the reason why
                            '// in some games you see distortion in the colours at screen edges.

                            '// As before when choosing the tile ID, we can use the bottom 12 bits of
                            '// the loopy register, but we need to make the implementation "coarser"
                            '// because instead of a specific tile, we want the attribute byte for a 
                            '// group of 4x4 tiles, Or in other words, we divide our 32x32 address
                            '// by 4 to give us an equivalent 8x8 address, And we offset this address
                            '// into the attribute section of the target nametable.

                            '// Reconstruct the 12 bit loopy address into an offset into the
                            '// attribute memory

                            '// "(vram_addr.coarse_x >> 2)"         Integer divide coarse x by 4, 
                            '//                                      from 5 bits to 3 bits
                            '// "((vram_addr.coarse_y >> 2) << 3)" : Integer divide coarse y by 4, 
                            '//                                      from 5 bits to 3 bits,
                            '//                                      shift to make room for coarse x

                            '// Result so far YX00 0yy yxxx

                            '// All attribute memory begins at 0x03C0 within a nametable, so Or with
                            '// result to select target nametable, And attribute byte offset. Finally
                            '// Or with 0x2000 to offset into nametable address space on PPU bus.
                            bg_next_tile_attrib = ppuRead(&H23C0US Or
                                                          MathHelpers.SafeShiftLeft16(vram_addr.NameTable_Y, 11) Or
                                                          MathHelpers.SafeShiftLeft16(vram_addr.NameTable_X, 10) Or
                                                          MathHelpers.SafeShiftLeft16(MathHelpers.SafeShiftRight16(vram_addr.Coarse_Y, 2), 3) Or
                                                          MathHelpers.SafeShiftRight16(vram_addr.Coarse_X, 2))

                            '// Right we've read the correct attribute byte for a specified address,
                            '// but the byte itself Is broken down further into the 2x2 tile groups
                            '// in the 4x4 attribute zone.

                            '// The attribute byte Is assembled thus BR(76) BL(54) TR(32) TL(10)
                            '//
                            '// +----+----+			    +----+----+
                            '// | TL | TR |			    | ID | ID |
                            '// +----+----+ where TL =  +----+----+
                            '// | BL | BR |			    | ID | ID |
                            '// +----+----+			    +----+----+
                            '//
                            '// Since we know we can access a tile directly from the 12 bit address, we
                            '// can analyse the bottom bits of the coarse coordinates to provide us with
                            '// the correct offset into the 8-bit word, to yield the 2 bits we are
                            '// actually interested in which specifies the palette for the 2x2 group of
                            '// tiles. We know if "coarse y % 4" < 2 we are in the top half else bottom half.
                            '// Likewise if "coarse x % 4" < 2 we are in the left half else right half.
                            '// Ultimately we want the bottom two bits of our attribute word to be the
                            '// palette selected. So shift as required...
                            If (vram_addr.Coarse_Y And &H2US) Then bg_next_tile_attrib = MathHelpers.SafeShiftRight16(bg_next_tile_attrib, 4)
                            If (vram_addr.Coarse_X And &H2US) Then bg_next_tile_attrib = MathHelpers.SafeShiftRight16(bg_next_tile_attrib, 2)
                            bg_next_tile_attrib = bg_next_tile_attrib And &H3UI
                            Exit Select
                            '// Compared to the last two, the next two are the easy ones... :P
                        Case 4
                            '// Fetch the next background tile LSB bit plane from the pattern memory
                            '// The Tile ID has been read from the nametable. We will use this id to 
                            '// index into the pattern memory to find the correct sprite (assuming
                            '// the sprites lie on 8x8 pixel boundaries in that memory, which they do
                            '// even though 8x16 sprites exist, as background tiles are always 8x8).
                            '//
                            '// Since the sprites are effectively 1 bit deep, but 8 pixels wide, we 
                            '// can represent a whole sprite row as a single byte, so offsetting
                            '// into the pattern memory Is easy. In total there Is 8KB so we need a 
                            '// 13 bit address.

                            '// "(control.pattern_background << 12)"   the pattern memory selector 
                            '//                                         from control register, either 0K
                            '//                                         Or 4K offset
                            '// "((uint16_t)bg_next_tile_id << 4)"    : the tile id multiplied by 16, as
                            '//                                         2 lots of 8 rows of 8 bit pixels
                            '// "(vram_addr.fine_y)"                  : Offset into which row based on
                            '//                                         vertical scroll offset
                            '// "+ 0"                                 : Mental clarity for plane offset
                            '// Note: No PPU address bus offset required as it starts at 0x0000
                            'bg_next_tile_lsb = ppuRead((PPUControl.pattern_background << 12) + (bg_next_tile_id << 4) + (vram_addr.Fine_Y) + 0) 'VBMATH checkme
                            bg_next_tile_lsb = ppuRead(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(MathHelpers.SafeShiftLeft16(PPUControl.pattern_background, 12), MathHelpers.SafeShiftLeft16(bg_next_tile_id, 4)), MathHelpers.SafeAddition16((vram_addr.Fine_Y), 0)))
                            Exit Select
                        Case 6
                            '// Fetch the next background tile MSB bit plane from the pattern memory
                            '// This Is the same as above, but has a +8 offset to select the next bit plane
                            'bg_next_tile_msb = ppuRead((PPUControl.pattern_background << 12) + (bg_next_tile_id << 4) + (vram_addr.Fine_Y) + 8) 'VBMATH checkme
                            bg_next_tile_msb = ppuRead(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(MathHelpers.SafeShiftLeft16(PPUControl.pattern_background, 12), MathHelpers.SafeShiftLeft16(bg_next_tile_id, 4)), MathHelpers.SafeAddition16((vram_addr.Fine_Y), 8))) 'VBMATH checkme
                            Exit Select
                        Case 7
                            '// Increment the background tile "pointer" to the next tile horizontally
                            '// in the nametable memory. Note this may cross nametable boundaries which
                            '// Is a little complex, but essential to implement scrolling
                            IncrementScrollX()
                            Exit Select
                    End Select
                End If
                '-----------------------------
                '// End of a visible scanline, so increment downwards...
                If cycle = 256 Then
                    IncrementScrollY()
                End If
                '-----------------------------
                '//...and reset the x position
                If cycle = 257 Then
                    LoadBackgroundShifters()
                    TransferAddressX()
                End If
                '-----------------------------
                '// Superfluous reads of tile id at end of scanline
                If cycle = 338 OrElse cycle = 340 Then
                    bg_next_tile_id = ppuRead(&H2000US Or (vram_addr.Reg And &HFFFUS))
                End If
                '-----------------------------
                If scanline = -1 AndAlso cycle >= 280 AndAlso cycle < 305 Then
                    '// End of vertical blank period so reset the Y address ready for rendering
                    TransferAddressY()
                End If
                '-----------------------------
                '// Foreground Rendering ========================================================
                '// I'm gonna cheat a bit here, which may reduce compatibility, but greatly
                '// simplifies delivering an intuitive understanding of what exactly Is going
                '// on. The PPU loads sprite information successively during the region that
                '// background tiles are Not being drawn. Instead, I'm going to perform
                '// all sprite evaluation in one hit. THE NES DOES Not DO IT Like THIS! This makes
                '// it easier to see the process of sprite evaluation.
                If cycle = 257 AndAlso scanline >= 0 Then
                    '// We've reached the end of a visible scanline. It is now time to determine
                    '// which sprites are visible on the next scanline, And preload this info
                    '// into buffers that we can work with while the scanline scans the row.

                    '// Firstly, clear out the sprite memory. This memory Is used to store the
                    '// sprites to be rendered. It Is Not the OAM.
                    For i As UInt32 = 0 To spriteScanline.Length() - 1 'VB
                        spriteScanline(i).MemorySet(&HFFUI)
                    Next

                    '// The NES supports a maximum number of sprites per scanline. Nominally
                    '// this Is 8 Or fewer sprites. This Is why in some games you see sprites
                    '// flicker Or disappear when the scene gets busy.
                    sprite_count = 0

                    '// Secondly, clear out any residual information in sprite pattern shifters
                    For i As UInt32 = 0 To 7 'VB
                        sprite_shifter_pattern_lo(i) = 0
                        sprite_shifter_pattern_hi(i) = 0
                    Next

                    '// Thirdly, Evaluate which sprites are visible in the next scanline. We need
                    '// to iterate through the OAM until we have found 8 sprites that have Y-positions
                    '// And heights that are within vertical range of the next scanline. Once we have
                    '// found 8 Or exhausted the OAM we stop. Now, notice I count to 9 sprites. This
                    '// Is so I can set the sprite overflow flag in the event of there being > 8 sprites.
                    Dim nOAMEntry As Byte = 0

                    '// New set of sprites. Sprite zero may Not exist in the New set, so clear this
                    '// flag.
                    bSpriteZeroHitPossible = False

                    'Dim diff As Int16 = 0
                    While nOAMEntry < 64 AndAlso sprite_count < 9
                        '// Note the conversion to signed numbers here
                        Dim diff As Int16 = Int16.Parse(scanline) - OAM(nOAMEntry).y

                        '// If the difference Is positive then the scanline Is at least at the
                        '// same height as the sprite, so check if it resides in the sprite vertically
                        '// depending on the current "sprite height mode"
                        '// FLAGGED

                        'Dim iif_var As Integer = IIf(PPUControl.sprite_size, 16, 8) 'was checking
                        If diff >= 0 AndAlso diff < IIf(PPUControl.sprite_size, 16, 8) AndAlso sprite_count < 8 Then
                            '// Sprite Is visible, so copy the attribute entry over to our
                            '// scanline sprite cache. Ive added < 8 here to guard the array
                            '// being written to.
                            If sprite_count < 8 Then
                                '// Is this sprite sprite zero?
                                If nOAMEntry = 0 Then
                                    '// It Is, so its possible it may trigger a 
                                    '// sprite zero hit when drawn
                                    bSpriteZeroHitPossible = True
                                End If
                                spriteScanline(sprite_count).Copy(OAM(nOAMEntry)) 'Copied
                            End If
                            sprite_count += 1
                            'diff += 1
                        End If
                        nOAMEntry += 1
                    End While '// End of sprite evaluation for next scanline

                    '// Set sprite overflow flag
                    PPUStatus.sprite_overflow = (sprite_count >= 8)

                    '// Now we have an array of the 8 visible sprites for the next scanline. By 
                    '// the nature of this search, they are also ranked in priority, because
                    '// those lower down in the OAM have the higher priority.

                    '// We also guarantee that "Sprite Zero" will exist in spriteScanline[0] if
                    '// it Is evaluated to be visible. 
                End If
                '-----------------------------
                If cycle = 340 Then
                    '// Now we're at the very end of the scanline, I'm going to prepare the 
                    '// sprite shifters with the 8 Or less selected sprites.
                    If sprite_count = 1 Then
                        sprite_count += 1
                        sprite_count -= 1
                    End If
                    If sprite_count > 0 Then 'Because if left as 0 the sprite count will throw an over flow (VB For Loops Problem not being able to if i < value)
                        For i As Byte = 0 To sprite_count - 1 'VB
                            '// We need to extract the 8-bit row patterns of the sprite with the
                            '// correct vertical offset. The "Sprite Mode" also affects this as
                            '// the sprites may be 8 Or 16 rows high. Additionally, the sprite
                            '// can be flipped both vertically And horizontally. So there's a lot
                            '// going on here :P

                            Dim sprite_pattern_bits_lo, sprite_pattern_bits_hi As Byte
                            Dim sprite_pattern_addr_lo, sprite_pattern_addr_hi As UInt16

                            '// Determine the memory addresses that contain the byte of pattern data. We
                            '// only need the lo pattern address, because the hi pattern address Is always
                            '// offset by 8 from the lo address.
                            If Not PPUControl.sprite_size Then
                                '// 8x8 Sprite Mode - The control register determines the pattern table
                                If Not (spriteScanline(i).attribute And &H80UI) Then
                                    '// Sprite is NOT flipped vertically, i.e. normal    
                                    sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(PPUControl.pattern_sprite, 12) Or  '// Which Pattern Table? 0KB or 4KB offset
                                                         MathHelpers.SafeShiftLeft16(spriteScanline(i).id, 4) Or        '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                         ((scanline - spriteScanline(i).y) And &HFFFFUS)                '// Which Row in cell? (0->7)
                                Else
                                    '// Sprite is flipped vertically, i.e. upside down
                                    sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(PPUControl.pattern_sprite, 12) Or                  '// Which Pattern Table? 0KB or 4KB offset
                                                         MathHelpers.SafeShiftLeft16(spriteScanline(i).id, 4) Or                        '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                         MathHelpers.SafeSubtract16(7, ((scanline - spriteScanline(i).y) And &HFFFFUS)) '// Which Row in cell? (7->0)
                                End If
                            Else
                                '// 8x16 Sprite Mode - The sprite attribute determines the pattern table
                                If Not (spriteScanline(i).attribute And &H80UI) Then
                                    '// Sprite is NOT flipped vertically, i.e. normal
                                    If (scanline - spriteScanline(i).y) < 8 Then
                                        '// Reading Top half Tile
                                        sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16((spriteScanline(i).id And &H1UI), 12) Or   '// Which Pattern Table? 0KB or 4KB offset
                                                             MathHelpers.SafeShiftLeft16((spriteScanline(i).id And &HFEUI), 4) Or   '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                             ((scanline - spriteScanline(i).y) And &H7US)                           '// Which Row in cell? (0->7)
                                    Else
                                        '// Reading Bottom Half Tile
                                        sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16((spriteScanline(i).id And &H1UI), 12) Or                                   '// Which Pattern Table? 0KB or 4KB offset
                                                             MathHelpers.SafeShiftLeft16(MathHelpers.SafeAddition16((spriteScanline(i).id And &HFEUI), 1), 4) Or    '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                             ((scanline - spriteScanline(i).y) And &H7UI)                                                           '// Which Row in cell? (0->7)
                                    End If
                                Else
                                    '// Sprite is flipped vertically, i.e. upside down
                                    If (scanline - spriteScanline(i).y < 8) Then
                                        sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16((spriteScanline(i).id And &H1UI), 12) Or                                   '// Which Pattern Table? 0KB or 4KB offset
                                                             MathHelpers.SafeShiftLeft16(MathHelpers.SafeAddition16((spriteScanline(i).id And &HFEUI), 1), 4) Or    '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                             MathHelpers.SafeSubtract16(7, (scanline - spriteScanline(i).y) And &H7UI)                              '// Which Row in cell? (0->7)

                                    Else
                                        sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16((spriteScanline(i).id And &H1UI), 12) Or       '// Which Pattern Table? 0KB or 4KB offset
                                                             MathHelpers.SafeShiftLeft16((spriteScanline(i).id And &HFEUI), 4) Or       '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                             MathHelpers.SafeSubtract16(7, (scanline - spriteScanline(i).y) And &H7UI)  '// Which Row in cell? (0->7)
                                    End If
                                End If
                            End If

                            '// Phew... XD I'm absolutely certain you can use some fantastic bit 
                            '// manipulation to reduce all of that to a few one liners, but in this
                            '// form it's easy to see the processes required for the different
                            '// sizes And vertical orientations

                            '// Hi bit plane equivalent Is always offset by 8 bytes from lo bit plane
                            sprite_pattern_addr_hi = MathHelpers.SafeAddition16(sprite_pattern_addr_lo, 8)

                            '// Now we have the address of the sprite patterns, we can read them
                            sprite_pattern_bits_lo = ppuRead(sprite_pattern_addr_lo)
                            sprite_pattern_bits_hi = ppuRead(sprite_pattern_addr_hi)

                            '// If the sprite Is flipped horizontally, we need to flip the 
                            '// pattern bytes.
                            If spriteScanline(i).attribute And &H40UI Then
                                '// This little lambda function "flips" a byte
                                '// so 0b11100000 becomes 0b00000111. It's very
                                '// clever, And stolen completely from here:
                                '// https//stackoverflow.com/a/2602885
                                Dim flipbyte = Function(ByVal b1 As Byte) As Byte
                                                   Dim b As Byte = b1
                                                   b = ((b And &HF0UI) >> 4) Or ((b And &HFUI) << 4)
                                                   b = ((b And &HCCUI) >> 2) Or ((b And &H33UI) << 2)
                                                   b = ((b And &HAAUI) >> 1) Or ((b And &H55UI) << 1)
                                                   Return b
                                               End Function
                                '// Flip Patterns Horizontally
                                sprite_pattern_bits_lo = flipbyte(sprite_pattern_bits_lo)
                                sprite_pattern_bits_hi = flipbyte(sprite_pattern_bits_hi)
                            End If
                            '// Finally! We can load the pattern into our sprite shift registers
                            '// ready for rendering on the next scanline
                            sprite_shifter_pattern_lo(i) = sprite_pattern_bits_lo
                            sprite_shifter_pattern_hi(i) = sprite_pattern_bits_hi
                        Next
                    End If
                End If
            End If
            '-----------------------------
            If scanline = 240 Then
                '// Post Render Scanline - Do Nothing!
            End If
            '-----------------------------
            If scanline >= 241 AndAlso scanline < 261 Then
                If scanline = 241 AndAlso cycle = 1 Then
                    '// Effectively end of frame, so set vertical blank flag
                    PPUStatus.vertical_blank = 1

                    '// If the control register tells us to emit a NMI when
                    '// entering vertical blanking period, do it! The CPU
                    '// will be informed that rendering Is complete so it can
                    '// perform operations with the PPU knowing it wont
                    '// produce visible artefacts
                    If PPUControl.enable_nmi Then
                        nmi = True
                    End If
                End If
            End If
            '-----------------------------
            '// Composition - We now have background & foreground pixel information for this cycle
            '// Background =============================================================
            Dim bg_pixel As Byte = &H0UI   ' 2-bit pixel to be rendered
            Dim bg_palette As Byte = &H0UI ' 3-bit index of the palette the pixel indexes [say what?]

            '// We only render backgrounds if the PPU Is enabled to do so. Note if 
            '// background rendering Is disabled, the pixel And palette combine
            '// to form 0x00. This will fall through the colour tables to yield
            '// the current background colour in effect
            If PPUMask.render_background Then
                If PPUMask.render_background_left OrElse (cycle >= 9) Then
                    '// Handle Pixel Selection by selecting the relevant bit
                    '// depending upon fine x scolling. This has the effect of
                    '// offsetting ALL background rendering by a set number
                    '// of pixels, permitting smooth scrolling
                    Dim bit_mux As UInt16 = MathHelpers.SafeShiftRight16(&H8000US, fine_x)

                    '// Select Plane pixels by extracting from the shifter 
                    '// at the required location. 
                    Dim p0_pixel As Byte = (bg_shifter_pattern_lo And bit_mux) > 0
                    Dim p1_pixel As Byte = (bg_shifter_pattern_hi And bit_mux) > 0

                    '// Combine to form pixel index
                    bg_pixel = MathHelpers.SafeShiftLeft8(p1_pixel, 1) Or p0_pixel

                    '// Get palette
                    Dim bg_pal0 As Byte = (bg_shifter_attrib_lo And bit_mux) > 0
                    Dim bg_pal1 As Byte = (bg_shifter_attrib_hi And bit_mux) > 0
                    '-----------------------------
                    bg_palette = MathHelpers.SafeShiftLeft8(bg_pal1, 1) Or bg_pal0
                End If
            End If
            '-----------------------------
            '// Foreground =============================================================
            '-----------------------------
            Dim fg_pixel As Byte = 0    '// The 2-bit pixel to be rendered
            Dim fg_palette As Byte = 0  '// The 3-bit index of the palette the pixel indexes
            Dim fg_priority As Byte = 0 '// A bit of the sprite attribute indicates if its
            '                           '// more important than the background

            If PPUMask.render_sprites Then
                '// Iterate through all sprites for this scanline. This Is to maintain
                '// sprite priority. As soon as we find a non transparent pixel of
                '// a sprite we can abort
                If PPUMask.render_sprites_left OrElse cycle >= 9 Then

                    bSpriteZeroBeingRendered = False

                    If sprite_count > 0 Then
                        For i As Byte = 0 To sprite_count - 1 'VB
                            '// Scanline cycle has "collided" with sprite, shifters taking over
                            If spriteScanline(i).x = 0 Then
                                '// Note Fine X scrolling does Not apply to sprites, the game
                                '// should maintain their relationship with the background. So
                                '// we'll just use the MSB of the shifter

                                '// Determine the pixel value...
                                Dim fg_pixel_lo As Byte = (sprite_shifter_pattern_lo(i) And &H80UI) > 0
                                Dim fg_pixel_hi As Byte = (sprite_shifter_pattern_hi(i) And &H80UI) > 0
                                fg_pixel = MathHelpers.SafeShiftLeft8(fg_pixel_hi, 1) Or fg_pixel_lo

                                '// Extract the palette from the bottom two bits. Recall
                                '// that foreground palettes are the latter 4 in the 
                                '// palette memory.
                                fg_palette = MathHelpers.SafeAddition8((spriteScanline(i).attribute And &H3UI), &H4UI)
                                fg_priority = ((spriteScanline(i).attribute And &H20UI) = 0)

                                '// If pixel Is Not transparent, we render it, And dont
                                '// bother checking the rest because the earlier sprites
                                '// in the list are higher priority
                                If fg_pixel <> 0 Then
                                    If i = 0 Then '// Is this sprite zero?
                                        bSpriteZeroBeingRendered = True
                                    End If
                                    Exit For 'break;
                                End If
                                '-----------------------------
                            End If
                        Next
                    End If
                End If
            End If

            '-----------------------------
            '// Now we have a background pixel And a foreground pixel. They need
            '// to be combined. It Is possible for sprites to go behind background
            '// tiles that are Not "transparent", yet another neat trick of the PPU
            '// that adds complexity for us poor emulator developers...
            '-----------------------------
            Dim pixel As Byte = 0   '// The FINAL Pixel...
            Dim palette As Byte = 0 '// The FINAL Palette...

            If bg_pixel = 0 AndAlso fg_pixel = 0 Then
                '// The background pixel Is transparent
                '// The foreground pixel Is transparent
                '// No winner, draw "background" colour
                pixel = &H0UI
                palette = &H0UI
            ElseIf bg_pixel = 0 AndAlso fg_pixel > 0 Then
                '// The background pixel Is transparent
                '// The foreground pixel Is visible
                '// Foreground wins!
                pixel = fg_pixel
                palette = fg_palette
            ElseIf bg_pixel > 0 AndAlso fg_pixel = 0 Then
                '// The background pixel Is visible
                '// The foreground pixel Is transparent
                '// Background wins!
                pixel = bg_pixel
                palette = bg_palette
            ElseIf bg_pixel > 0 AndAlso fg_pixel > 0 Then
                '// The background pixel Is visible
                '// The foreground pixel Is visible
                '// Hmmm...
                If fg_priority Then
                    '// Foreground cheats its way to victory!
                    pixel = fg_pixel
                    palette = fg_palette
                Else
                    '// Background is considered more important!
                    pixel = bg_pixel
                    palette = bg_palette
                End If

                '-----------------------------
                ' Sprite Zero Hit Detection
                '-----------------------------
                If bSpriteZeroHitPossible AndAlso bSpriteZeroBeingRendered Then
                    '// Sprite zero Is a collision between foreground And background
                    '// so they must both be enabled
                    If PPUMask.render_background And PPUMask.render_sprites Then
                        '// The left edge of the screen has specific switches to control
                        '// its appearance. This Is used to smooth inconsistencies when
                        '// scrolling (since sprites x coord must be >= 0)
                        If Not (PPUMask.render_background_left Or PPUMask.render_sprites_left) Then
                            If cycle >= 9 AndAlso cycle < 258 Then
                                PPUStatus.sprite_zero_hit = 1
                            End If
                        Else
                            If cycle >= 1 AndAlso cycle < 258 Then
                                PPUStatus.sprite_zero_hit = 1
                            End If
                        End If
                    End If
                End If
                '-----------------------------
            End If

            '// Now we have a final pixel colour, And a palette for this cycle
            '// of the current scanline. Let's at long last, draw that ^&%*er :P
            sprScreen.SetPixel(cycle - 1, scanline, GetColorFromPaletteRam(palette, pixel))

            '// Advance renderer - it never stops, it's relentless
            cycle += 1
            If PPUMask.render_background OrElse PPUMask.render_sprites Then
                If cycle = 260 AndAlso scanline < 240 Then
                    Cart.GetMapper.Scanline()
                End If
            End If
            '-----------------------------
            If cycle >= 341 Then
                cycle = 0
                scanline += 1
                If scanline >= 261 Then
                    scanline = -1
                    frame_complete = True
                    odd_frame = Not odd_frame
                End If
            End If

        End Sub

    End Class

End Namespace