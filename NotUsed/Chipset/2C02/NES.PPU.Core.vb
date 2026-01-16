Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' NES 2C02 Picture Processing Unit - Core (Partial Class)
    ''' Modern, high-performance implementation split across multiple files
    ''' </summary>
    Partial Public NotInheritable Class PPU2C02
        Implements IDisposable

#Region "Constants"
        Private Const NAMETABLE_SIZE As Integer = 1024
        Private Const PATTERN_TABLE_SIZE As Integer = 4096
        Private Const PALETTE_SIZE As Integer = 32
        Private Const SCREEN_WIDTH As Integer = 256
        Private Const SCREEN_HEIGHT As Integer = 240
        Private Const PATTERN_TABLE_WIDTH As Integer = 128
        Private Const PATTERN_TABLE_HEIGHT As Integer = 128
        Private Const OAM_SIZE As Integer = 64
        Private Const SPRITE_SCANLINE_SIZE As Integer = 8
#End Region

#Region "Memory"
        ' Nametables (2KB total, 2x 1KB tables)
        Private ReadOnly _nametable0(NAMETABLE_SIZE - 1) As Byte
        Private ReadOnly _nametable1(NAMETABLE_SIZE - 1) As Byte

        ' Palette RAM (32 bytes)
        Private ReadOnly _paletteRam(PALETTE_SIZE - 1) As Byte

        ' Pattern tables (8KB total, 2x 4KB tables) - usually in cart CHR ROM
        Private ReadOnly _patternTable0(PATTERN_TABLE_SIZE - 1) As Byte
        Private ReadOnly _patternTable1(PATTERN_TABLE_SIZE - 1) As Byte

        ' NES color palette (64 colors)
        Private ReadOnly _systemPalette(63) As GraphicsObjects.Pixel
#End Region

#Region "Rendering Surfaces"
        ' Main screen output
        Private ReadOnly _screen As GraphicsObjects.Sprite

        ' Debug visualization surfaces
        Private ReadOnly _nameTableSprites(1) As GraphicsObjects.Sprite
        Private ReadOnly _patternTableSprites(1) As GraphicsObjects.Sprite
#End Region

#Region "Registers"
        ' PPU registers
        Private _control As PpuControlRegister
        Private _mask As PpuMaskRegister
        Private _status As PpuStatusRegister

        ' Loopy scroll registers
        Private _vramAddr As LoopyRegister     ' Current VRAM address
        Private _tramAddr As LoopyRegister     ' Temporary VRAM address

        ' Internal state
        Private _fineX As Byte                  ' Fine X scroll (3 bits)
        Private _addressLatch As Byte          ' First/second write toggle
        Private _dataBuffer As Byte            ' PPU data read buffer
#End Region

#Region "Timing"
        Private _scanline As Short             ' Current scanline (-1 to 260)
        Private _cycle As Short                ' Current cycle (0 to 340)
        Private _oddFrame As Boolean           ' Odd/even frame toggle
        Private _frameComplete As Boolean
        Private _isDisposed As Boolean
#End Region

#Region "Background Rendering State"
        ' Next tile data
        Private _bgNextTileId As Byte
        Private _bgNextTileAttrib As Byte
        Private _bgNextTileLsb As Byte
        Private _bgNextTileMsb As Byte

        ' Shift registers
        Private _bgShifterPatternLo As UShort
        Private _bgShifterPatternHi As UShort
        Private _bgShifterAttribLo As UShort
        Private _bgShifterAttribHi As UShort

        ' Attribute shift lookup table [coarse_y bit 1][coarse_x bit 1]
        Private ReadOnly _attrShiftTable(1, 1) As Byte
#End Region

#Region "Sprite Rendering State"
        ' OAM (Object Attribute Memory) - 64 sprites
        Public ReadOnly OAM(OAM_SIZE - 1) As OAMEntry

        Private _oamAddress As Byte            ' OAM address register

        ' Sprite scanline buffer (8 sprites max per scanline)
        Private _spriteScanline(SPRITE_SCANLINE_SIZE - 1) As OAMEntry
        Private _spriteCount As Byte

        ' Sprite shifters
        Private _spriteShifterLo(SPRITE_SCANLINE_SIZE - 1) As Byte
        Private _spriteShifterHi(SPRITE_SCANLINE_SIZE - 1) As Byte

        ' Sprite zero hit detection
        Private _spriteZeroHitPossible As Boolean
        Private _spriteZeroBeingRendered As Boolean
#End Region

#Region "Interrupts"
        Public NmiRequested As Boolean
        Public ScanlineTrigger As Boolean
#End Region

#Region "Properties"
        Public ReadOnly Property FrameComplete As Boolean
            Get
                Return _frameComplete
            End Get
        End Property

        Public ReadOnly Property CurrentScanline As Short
            Get
                Return _scanline
            End Get
        End Property

        Public ReadOnly Property CurrentCycle As Short
            Get
                Return _cycle
            End Get
        End Property

        ''' <summary>Get the main screen output</summary>
        Public ReadOnly Property Screen As GraphicsObjects.Sprite
            Get
                Return _screen
            End Get
        End Property

        ''' <summary>Get nametable visualization</summary>
        Public Function GetNameTable(index As Integer) As GraphicsObjects.Sprite
            If index >= 0 AndAlso index <= 1 Then
                Return _nameTableSprites(index)
            End If
            Return Nothing
        End Function
#End Region

#Region "Constructor & Initialization"
        Public Sub New()
            ' Initialize rendering surfaces
            _screen = New GraphicsObjects.Sprite(SCREEN_WIDTH, SCREEN_HEIGHT)
            _nameTableSprites(0) = New GraphicsObjects.Sprite(SCREEN_WIDTH, SCREEN_HEIGHT)
            _nameTableSprites(1) = New GraphicsObjects.Sprite(SCREEN_WIDTH, SCREEN_HEIGHT)
            _patternTableSprites(0) = New GraphicsObjects.Sprite(PATTERN_TABLE_WIDTH, PATTERN_TABLE_HEIGHT)
            _patternTableSprites(1) = New GraphicsObjects.Sprite(PATTERN_TABLE_WIDTH, PATTERN_TABLE_HEIGHT)

            ' Initialize NES color palette
            InitializeSystemPalette()

            ' Initialize attribute shift lookup table
            _attrShiftTable(0, 0) = 0  ' Top-left
            _attrShiftTable(0, 1) = 2  ' Top-right
            _attrShiftTable(1, 0) = 4  ' Bottom-left
            _attrShiftTable(1, 1) = 6  ' Bottom-right

            ' Reset to initial state
            Reset()
        End Sub

        Private Sub InitializeSystemPalette()
            ' NES NTSC palette (64 colors)
            _systemPalette(&H0) = New GraphicsObjects.Pixel(84, 84, 84)
            _systemPalette(&H1) = New GraphicsObjects.Pixel(0, 30, 116)
            _systemPalette(&H2) = New GraphicsObjects.Pixel(8, 16, 144)
            _systemPalette(&H3) = New GraphicsObjects.Pixel(48, 0, 136)
            _systemPalette(&H4) = New GraphicsObjects.Pixel(68, 0, 100)
            _systemPalette(&H5) = New GraphicsObjects.Pixel(92, 0, 48)
            _systemPalette(&H6) = New GraphicsObjects.Pixel(84, 4, 0)
            _systemPalette(&H7) = New GraphicsObjects.Pixel(60, 24, 0)
            _systemPalette(&H8) = New GraphicsObjects.Pixel(32, 42, 0)
            _systemPalette(&H9) = New GraphicsObjects.Pixel(8, 58, 0)
            _systemPalette(&HA) = New GraphicsObjects.Pixel(0, 64, 0)
            _systemPalette(&HB) = New GraphicsObjects.Pixel(0, 60, 0)
            _systemPalette(&HC) = New GraphicsObjects.Pixel(0, 50, 60)
            _systemPalette(&HD) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&HE) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&HF) = New GraphicsObjects.Pixel(0, 0, 0)

            _systemPalette(&H10) = New GraphicsObjects.Pixel(152, 150, 152)
            _systemPalette(&H11) = New GraphicsObjects.Pixel(8, 76, 196)
            _systemPalette(&H12) = New GraphicsObjects.Pixel(48, 50, 236)
            _systemPalette(&H13) = New GraphicsObjects.Pixel(92, 30, 228)
            _systemPalette(&H14) = New GraphicsObjects.Pixel(136, 20, 176)
            _systemPalette(&H15) = New GraphicsObjects.Pixel(160, 20, 100)
            _systemPalette(&H16) = New GraphicsObjects.Pixel(152, 34, 32)
            _systemPalette(&H17) = New GraphicsObjects.Pixel(120, 60, 0)
            _systemPalette(&H18) = New GraphicsObjects.Pixel(84, 90, 0)
            _systemPalette(&H19) = New GraphicsObjects.Pixel(40, 114, 0)
            _systemPalette(&H1A) = New GraphicsObjects.Pixel(8, 124, 0)
            _systemPalette(&H1B) = New GraphicsObjects.Pixel(0, 118, 40)
            _systemPalette(&H1C) = New GraphicsObjects.Pixel(0, 102, 120)
            _systemPalette(&H1D) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H1E) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H1F) = New GraphicsObjects.Pixel(0, 0, 0)

            _systemPalette(&H20) = New GraphicsObjects.Pixel(236, 238, 236)
            _systemPalette(&H21) = New GraphicsObjects.Pixel(76, 154, 236)
            _systemPalette(&H22) = New GraphicsObjects.Pixel(120, 124, 236)
            _systemPalette(&H23) = New GraphicsObjects.Pixel(176, 98, 236)
            _systemPalette(&H24) = New GraphicsObjects.Pixel(228, 84, 236)
            _systemPalette(&H25) = New GraphicsObjects.Pixel(236, 88, 180)
            _systemPalette(&H26) = New GraphicsObjects.Pixel(236, 106, 100)
            _systemPalette(&H27) = New GraphicsObjects.Pixel(212, 136, 32)
            _systemPalette(&H28) = New GraphicsObjects.Pixel(160, 170, 0)
            _systemPalette(&H29) = New GraphicsObjects.Pixel(116, 196, 0)
            _systemPalette(&H2A) = New GraphicsObjects.Pixel(76, 208, 32)
            _systemPalette(&H2B) = New GraphicsObjects.Pixel(56, 204, 108)
            _systemPalette(&H2C) = New GraphicsObjects.Pixel(56, 180, 204)
            _systemPalette(&H2D) = New GraphicsObjects.Pixel(60, 60, 60)
            _systemPalette(&H2E) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H2F) = New GraphicsObjects.Pixel(0, 0, 0)

            _systemPalette(&H30) = New GraphicsObjects.Pixel(236, 238, 236)
            _systemPalette(&H31) = New GraphicsObjects.Pixel(168, 204, 236)
            _systemPalette(&H32) = New GraphicsObjects.Pixel(188, 188, 236)
            _systemPalette(&H33) = New GraphicsObjects.Pixel(212, 178, 236)
            _systemPalette(&H34) = New GraphicsObjects.Pixel(236, 174, 236)
            _systemPalette(&H35) = New GraphicsObjects.Pixel(236, 174, 212)
            _systemPalette(&H36) = New GraphicsObjects.Pixel(236, 180, 176)
            _systemPalette(&H37) = New GraphicsObjects.Pixel(228, 196, 144)
            _systemPalette(&H38) = New GraphicsObjects.Pixel(204, 210, 120)
            _systemPalette(&H39) = New GraphicsObjects.Pixel(180, 222, 120)
            _systemPalette(&H3A) = New GraphicsObjects.Pixel(168, 226, 144)
            _systemPalette(&H3B) = New GraphicsObjects.Pixel(152, 226, 180)
            _systemPalette(&H3C) = New GraphicsObjects.Pixel(160, 214, 228)
            _systemPalette(&H3D) = New GraphicsObjects.Pixel(160, 162, 160)
            _systemPalette(&H3E) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H3F) = New GraphicsObjects.Pixel(0, 0, 0)
        End Sub

        Public Sub Reset()
            ' Clear OAM
            For i = 0 To OAM_SIZE - 1
                OAM(i).Fill(&HFF)
            Next
            _oamAddress = 0

            ' Reset registers
            _control.Reg = 0
            _mask.Reg = 0
            _status.Reg = 0
            _vramAddr.Reg = 0
            _tramAddr.Reg = 0

            ' Reset internal state
            _fineX = 0
            _addressLatch = 0
            _dataBuffer = 0

            ' Reset timing
            _scanline = 0
            _cycle = 0
            _oddFrame = False
            _frameComplete = False

            ' Reset background state
            _bgNextTileId = 0
            _bgNextTileAttrib = 0
            _bgNextTileLsb = 0
            _bgNextTileMsb = 0
            _bgShifterPatternLo = 0
            _bgShifterPatternHi = 0
            _bgShifterAttribLo = 0
            _bgShifterAttribHi = 0

            ' Reset sprite state
            _spriteCount = 0
            _spriteZeroHitPossible = False
            _spriteZeroBeingRendered = False

            ' Reset interrupts
            NmiRequested = False
            ScanlineTrigger = False
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            If Not _isDisposed Then
                _screen?.Dispose()
                _nameTableSprites(0)?.Dispose()
                _nameTableSprites(1)?.Dispose()
                _patternTableSprites(0)?.Dispose()
                _patternTableSprites(1)?.Dispose()
                _isDisposed = True
            End If
        End Sub
#End Region

    End Class

End Namespace