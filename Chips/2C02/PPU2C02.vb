Imports System.Runtime.CompilerServices

Namespace NintendoEntertainmentSystem

    ''' <summary>
    ''' NES 2C02 Picture Processing Unit
    ''' Modernized implementation with improved code organization
    ''' </summary>
    Public NotInheritable Class NetPPU2C02
        Implements IDisposable

#Region "Memory & Rendering Surfaces"
        Private ReadOnly _nametable0(1023) As Byte
        Private ReadOnly _nametable1(1023) As Byte
        Private ReadOnly _paletteRam(31) As Byte
        Private ReadOnly _patternTable0(4095) As Byte
        Private ReadOnly _patternTable1(4095) As Byte
        Private ReadOnly _systemPalette(63) As GraphicsObjects.Pixel
        Private ReadOnly _screen As GraphicsObjects.Sprite
        Private ReadOnly _nameTableSprites(1) As GraphicsObjects.Sprite
        Private ReadOnly _patternTableSprites(1) As GraphicsObjects.Sprite
#End Region

#Region "Registers & State"
        Private _control As PpuControlRegister
        Private _mask As PpuMaskRegister
        Private _status As PpuStatusRegister
        Private _vramAddr As LoopyRegister
        Private _tramAddr As LoopyRegister
        Private _fineX As Byte
        Private _addressLatch As Byte
        Private _dataBuffer As Byte
        Private _scanline As Short
        Private _cycle As Short
        Private _oddFrame As Boolean
        Private _frameComplete As Boolean
        Private _isDisposed As Boolean
#End Region

#Region "Background Rendering"
        Private _bgNextTileId As Byte
        Private _bgNextTileAttrib As Byte
        Private _bgNextTileLsb As Byte
        Private _bgNextTileMsb As Byte
        Private _bgShifterPatternLo As UShort
        Private _bgShifterPatternHi As UShort
        Private _bgShifterAttribLo As UShort
        Private _bgShifterAttribHi As UShort
#End Region

#Region "Sprite Rendering"
        Public ReadOnly OAM(63) As OAMEntry
        Private _oamAddress As Byte
        Private _spriteScanline(7) As OAMEntry
        Private _spriteCount As Byte
        Private _spriteShifterLo(7) As Byte
        Private _spriteShifterHi(7) As Byte
        Private _spriteZeroHitPossible As Boolean
        Private _spriteZeroBeingRendered As Boolean
#End Region

#Region "Public Interface"
        Public NmiRequested As Boolean
        Public ScanlineTrigger As Boolean

        Public Property FrameComplete As Boolean
            Get
                Return _frameComplete
            End Get
            Set(value As Boolean)
                _frameComplete = value
            End Set
        End Property

        Public ReadOnly Property Screen As GraphicsObjects.Sprite
            Get
                Return _screen
            End Get
        End Property

        Public Function GetNameTable(index As Integer) As GraphicsObjects.Sprite
            If index >= 0 AndAlso index <= 1 Then Return _nameTableSprites(index)
            Return Nothing
        End Function
#End Region

#Region "Constructor & Initialization"
        Public Sub New()
            _screen = New GraphicsObjects.Sprite(256, 240)
            _nameTableSprites(0) = New GraphicsObjects.Sprite(256, 240)
            _nameTableSprites(1) = New GraphicsObjects.Sprite(256, 240)
            _patternTableSprites(0) = New GraphicsObjects.Sprite(128, 128)
            _patternTableSprites(1) = New GraphicsObjects.Sprite(128, 128)

            InitializeSystemPalette()
            Reset()
        End Sub

        Private Sub InitializeSystemPalette()
            _systemPalette(&H0UI) = New GraphicsObjects.Pixel(84, 84, 84)
            _systemPalette(&H1UI) = New GraphicsObjects.Pixel(0, 30, 116)
            _systemPalette(&H2UI) = New GraphicsObjects.Pixel(8, 16, 144)
            _systemPalette(&H3UI) = New GraphicsObjects.Pixel(48, 0, 136)
            _systemPalette(&H4UI) = New GraphicsObjects.Pixel(68, 0, 100)
            _systemPalette(&H5UI) = New GraphicsObjects.Pixel(92, 0, 48)
            _systemPalette(&H6UI) = New GraphicsObjects.Pixel(84, 4, 0)
            _systemPalette(&H7UI) = New GraphicsObjects.Pixel(60, 24, 0)
            _systemPalette(&H8UI) = New GraphicsObjects.Pixel(32, 42, 0)
            _systemPalette(&H9UI) = New GraphicsObjects.Pixel(8, 58, 0)
            _systemPalette(&HAUI) = New GraphicsObjects.Pixel(0, 64, 0)
            _systemPalette(&HBUI) = New GraphicsObjects.Pixel(0, 60, 0)
            _systemPalette(&HCUI) = New GraphicsObjects.Pixel(0, 50, 60)
            _systemPalette(&HDUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&HEUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&HFUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H10UI) = New GraphicsObjects.Pixel(152, 150, 152)
            _systemPalette(&H11UI) = New GraphicsObjects.Pixel(8, 76, 196)
            _systemPalette(&H12UI) = New GraphicsObjects.Pixel(48, 50, 236)
            _systemPalette(&H13UI) = New GraphicsObjects.Pixel(92, 30, 228)
            _systemPalette(&H14UI) = New GraphicsObjects.Pixel(136, 20, 176)
            _systemPalette(&H15UI) = New GraphicsObjects.Pixel(160, 20, 100)
            _systemPalette(&H16UI) = New GraphicsObjects.Pixel(152, 34, 32)
            _systemPalette(&H17UI) = New GraphicsObjects.Pixel(120, 60, 0)
            _systemPalette(&H18UI) = New GraphicsObjects.Pixel(84, 90, 0)
            _systemPalette(&H19UI) = New GraphicsObjects.Pixel(40, 114, 0)
            _systemPalette(&H1AUI) = New GraphicsObjects.Pixel(8, 124, 0)
            _systemPalette(&H1BUI) = New GraphicsObjects.Pixel(0, 118, 40)
            _systemPalette(&H1CUI) = New GraphicsObjects.Pixel(0, 102, 120)
            _systemPalette(&H1DUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H1EUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H1FUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H20UI) = New GraphicsObjects.Pixel(236, 238, 236)
            _systemPalette(&H21UI) = New GraphicsObjects.Pixel(76, 154, 236)
            _systemPalette(&H22UI) = New GraphicsObjects.Pixel(120, 124, 236)
            _systemPalette(&H23UI) = New GraphicsObjects.Pixel(176, 98, 236)
            _systemPalette(&H24UI) = New GraphicsObjects.Pixel(228, 84, 236)
            _systemPalette(&H25UI) = New GraphicsObjects.Pixel(236, 88, 180)
            _systemPalette(&H26UI) = New GraphicsObjects.Pixel(236, 106, 100)
            _systemPalette(&H27UI) = New GraphicsObjects.Pixel(212, 136, 32)
            _systemPalette(&H28UI) = New GraphicsObjects.Pixel(160, 170, 0)
            _systemPalette(&H29UI) = New GraphicsObjects.Pixel(116, 196, 0)
            _systemPalette(&H2AUI) = New GraphicsObjects.Pixel(76, 208, 32)
            _systemPalette(&H2BUI) = New GraphicsObjects.Pixel(56, 204, 108)
            _systemPalette(&H2CUI) = New GraphicsObjects.Pixel(56, 180, 204)
            _systemPalette(&H2DUI) = New GraphicsObjects.Pixel(60, 60, 60)
            _systemPalette(&H2EUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H2FUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H30UI) = New GraphicsObjects.Pixel(236, 238, 236)
            _systemPalette(&H31UI) = New GraphicsObjects.Pixel(168, 204, 236)
            _systemPalette(&H32UI) = New GraphicsObjects.Pixel(188, 188, 236)
            _systemPalette(&H33UI) = New GraphicsObjects.Pixel(212, 178, 236)
            _systemPalette(&H34UI) = New GraphicsObjects.Pixel(236, 174, 236)
            _systemPalette(&H35UI) = New GraphicsObjects.Pixel(236, 174, 212)
            _systemPalette(&H36UI) = New GraphicsObjects.Pixel(236, 180, 176)
            _systemPalette(&H37UI) = New GraphicsObjects.Pixel(228, 196, 144)
            _systemPalette(&H38UI) = New GraphicsObjects.Pixel(204, 210, 120)
            _systemPalette(&H39UI) = New GraphicsObjects.Pixel(180, 222, 120)
            _systemPalette(&H3AUI) = New GraphicsObjects.Pixel(168, 226, 144)
            _systemPalette(&H3BUI) = New GraphicsObjects.Pixel(152, 226, 180)
            _systemPalette(&H3CUI) = New GraphicsObjects.Pixel(160, 214, 228)
            _systemPalette(&H3DUI) = New GraphicsObjects.Pixel(160, 162, 160)
            _systemPalette(&H3EUI) = New GraphicsObjects.Pixel(0, 0, 0)
            _systemPalette(&H3FUI) = New GraphicsObjects.Pixel(0, 0, 0)
        End Sub

        Public Sub Reset()
            For i = 0 To 63
                OAM(i).Fill(&HFFUI)
            Next
            _oamAddress = 0
            _control.Reg = 0
            _mask.Reg = 0
            _status.Reg = 0
            _vramAddr.Reg = 0
            _tramAddr.Reg = 0
            _fineX = 0
            _addressLatch = 0
            _dataBuffer = 0
            _scanline = 0
            _cycle = 0
            _oddFrame = False
            _frameComplete = False
            _bgNextTileId = 0
            _bgNextTileAttrib = 0
            _bgNextTileLsb = 0
            _bgNextTileMsb = 0
            _bgShifterPatternLo = 0
            _bgShifterPatternHi = 0
            _bgShifterAttribLo = 0
            _bgShifterAttribHi = 0
            _spriteCount = 0
            _spriteZeroHitPossible = False
            _spriteZeroBeingRendered = False
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

#Region "CPU Register Interface"
        Public Function CpuRead(addr As UShort, Optional rdOnly As Boolean = False) As Byte
            Dim data As Byte = 0

            If rdOnly Then
                Select Case addr
                    Case &H0US : data = _control.Reg
                    Case &H1US : data = _mask.Reg
                    Case &H2US : data = _status.Reg
                End Select
            Else
                Select Case addr
                    Case &H2US
                        data = (_status.Reg And &HE0UI) Or (_dataBuffer And &H1FUI)
                        _status.VerticalBlank = False
                        _addressLatch = 0

                    Case &H4US
                        data = OAM(_oamAddress \ 4).GetByteAt(_oamAddress)

                    Case &H7US
                        data = _dataBuffer
                        _dataBuffer = PpuRead(_vramAddr.Reg)
                        If _vramAddr.Reg >= &H3F00US Then data = _dataBuffer
                        _vramAddr.Reg += If(_control.IncrementMode, 32US, 1US)
                End Select
            End If

            Return data
        End Function

        Public Sub CpuWrite(addr As UShort, data As Byte)
            Select Case addr
                Case &H0US
                    _control.Reg = data
                    _tramAddr.NametableX = _control.NametableX
                    _tramAddr.NametableY = _control.NametableY

                Case &H1US
                    _mask.Reg = data

                Case &H3US
                    _oamAddress = data

                Case &H4US
                    OAM(_oamAddress \ 4).SetByteAt(_oamAddress, data)

                Case &H5US
                    If _addressLatch = 0 Then
                        _fineX = data And &H7UI
                        _tramAddr.CoarseX = data >> 3
                        _addressLatch = 1
                    Else
                        _tramAddr.FineY = data And &H7UI
                        _tramAddr.CoarseY = data >> 3
                        _addressLatch = 0
                    End If

                Case &H6US
                    If _addressLatch = 0 Then
                        _tramAddr.Reg = CUShort((CUShort(data And &H3FUI) << 8) Or (_tramAddr.Reg And &HFFUS))
                        _addressLatch = 1
                    Else
                        _tramAddr.Reg = (_tramAddr.Reg And &HFF00US) Or data
                        '_vramAddr.Reg = _tramAddr.Reg
                        _vramAddr = _tramAddr
                        _addressLatch = 0
                    End If

                Case &H7US
                    PpuWrite(_vramAddr.Reg, data)
                    _vramAddr.Reg += If(_control.IncrementMode, 32US, 1US)
            End Select
        End Sub
#End Region

#Region "PPU Bus Interface"
        Public Function PpuRead(addr As UShort, Optional rdOnly As Boolean = False) As Byte
            Dim data As Byte = 0
            addr = addr And &H3FFFUS

            If Cart.PpuRead(addr, data) Then
                ' Cartridge handled read
            ElseIf addr <= &H1FFFUS Then
                data = If(addr < &H1000US, _patternTable0(addr), _patternTable1(addr And &HFFFUS))
            ElseIf addr <= &H3EFFUS Then
                addr = addr And &HFFFUS
                Dim mirror = Cart.MirrorMode()

                If mirror = MirrorMode.Vertical Then
                    If addr < &H400US Then
                        data = _nametable0(addr)
                    ElseIf addr < &H800US Then
                        data = _nametable1(addr And &H3FFUS)
                    ElseIf addr < &HC00US Then
                        data = _nametable0(addr And &H3FFUS)
                    Else
                        data = _nametable1(addr And &H3FFUS)
                    End If
                Else
                    If addr < &H400US Then
                        data = _nametable0(addr)
                    ElseIf addr < &H800US Then
                        data = _nametable0(addr And &H3FFUS)
                    ElseIf addr < &HC00US Then
                        data = _nametable1(addr And &H3FFUS)
                    Else
                        data = _nametable1(addr And &H3FFUS)
                    End If
                End If
            Else
                addr = addr And &H1F
                If addr = &H10US Then addr = &H0US
                If addr = &H14US Then addr = &H4US
                If addr = &H18US Then addr = &H8US
                If addr = &H1CUS Then addr = &HCUS
                data = _paletteRam(addr) And If(_mask.Grayscale, &H30UI, &H3FUI)
            End If

            Return data
        End Function

        Public Sub PpuWrite(addr As UShort, data As Byte)
            addr = addr And &H3FFFUS

            If Cart.PpuWrite(addr, data) Then
                ' Cartridge handled write
            ElseIf addr <= &H1FFFUS Then
                If addr < &H1000US Then
                    _patternTable0(addr) = data
                Else
                    _patternTable1(addr And &HFFFUS) = data
                End If
            ElseIf addr <= &H3EFFUS Then
                addr = addr And &HFFFUS
                Dim mirror = Cart.MirrorMode()

                If mirror = MirrorMode.Vertical Then
                    If addr < &H400US Then
                        _nametable0(addr) = data
                    ElseIf addr < &H800US Then
                        _nametable1(addr And &H3FFUS) = data
                    ElseIf addr < &HC00US Then
                        _nametable0(addr And &H3FFUS) = data
                    Else
                        _nametable1(addr And &H3FFUS) = data
                    End If
                Else
                    If addr < &H400US Then
                        _nametable0(addr) = data
                    ElseIf addr < &H800US Then
                        _nametable0(addr And &H3FFUS) = data
                    ElseIf addr < &HC00US Then
                        _nametable1(addr And &H3FFUS) = data
                    Else
                        _nametable1(addr And &H3FFUS) = data
                    End If
                End If
            Else
                addr = addr And &H1FUS
                If addr = &H10US Then addr = &H0US
                If addr = &H14US Then addr = &H4US
                If addr = &H18US Then addr = &H8US
                If addr = &H1CUS Then addr = &HCUS
                _paletteRam(addr) = data
            End If
        End Sub

        'Public Function GetColorFromPaletteRam(palette As Byte, pixel As Byte) As GraphicsObjects.Pixel
        '    Return _systemPalette(PpuRead(&H3F00US + (palette << 2) + pixel) And &H3F)
        'End Function
        '
        'Private Function GetColorFromPalette(palette As Byte, pixel As Byte) As GraphicsObjects.Pixel
        '    Return GetColorFromPaletteRam(palette, pixel)
        'End Function

        Public Function GetColorFromPalette(palette As Byte, pixel As Byte) As GraphicsObjects.Pixel
            Return _systemPalette(PpuRead(&H3F00US + (CUShort(palette) << 2) + pixel) And &H3FUI)
        End Function
#End Region

#Region "Background Rendering Helpers"
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub IncrementScrollX()
            If Not _mask.RenderBackground AndAlso Not _mask.RenderSprites Then Return

            If _vramAddr.CoarseX = 31 Then
                _vramAddr.CoarseX = 0
                _vramAddr.NametableX = Not _vramAddr.NametableX
            Else
                _vramAddr.CoarseX += 1
            End If
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub IncrementScrollY()
            If Not _mask.RenderBackground AndAlso Not _mask.RenderSprites Then Return

            If _vramAddr.FineY < 7 Then
                _vramAddr.FineY += 1
            Else
                _vramAddr.FineY = 0
                If _vramAddr.CoarseY = 29 Then
                    _vramAddr.CoarseY = 0
                    _vramAddr.NametableY = Not _vramAddr.NametableY
                ElseIf _vramAddr.CoarseY = 31 Then
                    _vramAddr.CoarseY = 0
                Else
                    _vramAddr.CoarseY += 1
                End If
            End If
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub TransferAddressX()
            If Not _mask.RenderBackground AndAlso Not _mask.RenderSprites Then Return
            _vramAddr.NametableX = _tramAddr.NametableX
            _vramAddr.CoarseX = _tramAddr.CoarseX
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub TransferAddressY()
            If Not _mask.RenderBackground AndAlso Not _mask.RenderSprites Then Return
            _vramAddr.FineY = _tramAddr.FineY
            _vramAddr.NametableY = _tramAddr.NametableY
            _vramAddr.CoarseY = _tramAddr.CoarseY
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub LoadBackgroundShifters()
            _bgShifterPatternLo = (_bgShifterPatternLo And &HFF00US) Or _bgNextTileLsb
            _bgShifterPatternHi = (_bgShifterPatternHi And &HFF00US) Or _bgNextTileMsb
            _bgShifterAttribLo = (_bgShifterAttribLo And &HFF00US) Or If((_bgNextTileAttrib And 1) <> 0, &HFFUS, 0)
            _bgShifterAttribHi = (_bgShifterAttribHi And &HFF00US) Or If((_bgNextTileAttrib And 2) <> 0, &HFFUS, 0)
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub UpdateShifters()
            If _mask.RenderBackground Then
                _bgShifterPatternLo <<= 1
                _bgShifterPatternHi <<= 1
                _bgShifterAttribLo <<= 1
                _bgShifterAttribHi <<= 1
            End If

            If _mask.RenderSprites AndAlso _cycle >= 1 AndAlso _cycle < 258 Then
                For i = 0 To Math.Min(_spriteCount - 1, 7)
                    If _spriteScanline(i).X > 0 Then
                        _spriteScanline(i).X -= 1
                    Else
                        _spriteShifterLo(i) <<= 1
                        _spriteShifterHi(i) <<= 1
                    End If
                Next
            End If
        End Sub
#End Region

#Region "Sprite Rendering Helpers"
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub EvaluateSprites()
            For i = 0 To 7
                _spriteScanline(i).Fill(&HFF)
                _spriteShifterLo(i) = 0
                _spriteShifterHi(i) = 0
            Next

            _spriteCount = 0
            _spriteZeroHitPossible = False

            Dim entry As Byte = 0
            While entry < 64 AndAlso _spriteCount < 9
                Dim diff = (_scanline + 1) - CInt(OAM(entry).Y)
                Dim height = If(_control.SpriteSize, 16, 8)

                If diff >= 0 AndAlso diff < height Then
                    If _spriteCount < 8 Then
                        If entry = 0 Then _spriteZeroHitPossible = True
                        _spriteScanline(_spriteCount).CopyFrom(OAM(entry))
                        _spriteCount += 1
                    Else
                        _spriteCount += 1
                    End If
                End If
                entry += 1
            End While

            _status.SpriteOverflow = (_spriteCount > 8)
        End Sub


        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Sub LoadSpriteShifters()
            For i = 0 To Math.Min(_spriteCount - 1, 7)
                Dim patternLo, patternHi As Byte
                Dim addrLo, addrHi As UShort

                Dim spriteLine = (_scanline + 1) - CInt(_spriteScanline(i).Y)

                If Not _control.SpriteSize Then
                    ' 8x8 mode
                    Dim row = spriteLine
                    If _spriteScanline(i).IsFlippedVertically Then row = 7 - row

                    addrLo = (If(_control.PatternSprite, &H1000US, 0US)) Or
                     (CUShort(_spriteScanline(i).TileID) << 4) Or
                     CUShort(row)
                Else
                    ' 8x16 mode
                    Dim row = spriteLine
                    If _spriteScanline(i).IsFlippedVertically Then row = 15 - row

                    Dim bank = (_spriteScanline(i).TileID And 1) << 12
                    Dim tile = _spriteScanline(i).TileID And &HFEUI
                    If row >= 8 Then
                        tile += 1
                        row -= 8
                    End If

                    addrLo = CUShort(bank Or (tile << 4) Or row)
                End If

                addrHi = addrLo + 8
                patternLo = PpuRead(addrLo)
                patternHi = PpuRead(addrHi)

                If _spriteScanline(i).IsFlippedHorizontally Then
                    patternLo = FlipByte(patternLo)
                    patternHi = FlipByte(patternHi)
                End If

                _spriteShifterLo(i) = patternLo
                _spriteShifterHi(i) = patternHi
            Next
        End Sub

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Function FlipByte(b As Byte) As Byte
            If b = 0 Then Return 0
            b = ((b And &HF0UI) >> 4) Or ((b And &HFUI) << 4)
            b = ((b And &HCCUI) >> 2) Or ((b And &H33UI) << 2)
            b = ((b And &HAAUI) >> 1) Or ((b And &H55UI) << 1)
            Return b
        End Function
#End Region

#Region "Main Clock"
        Public Sub Clock()
            ' Visible scanlines + pre-render
            If _scanline >= -1 AndAlso _scanline < 240 Then

                If _scanline = 0 AndAlso _cycle = 0 AndAlso _oddFrame AndAlso (_mask.RenderBackground OrElse _mask.RenderSprites) Then
                    _cycle = 1
                End If

                If _scanline = -1 AndAlso _cycle = 1 Then
                    _status.VerticalBlank = False
                    _status.SpriteOverflow = False
                    _status.SpriteZeroHit = False

                    For i = 0 To 7
                        _spriteShifterLo(i) = 0
                        _spriteShifterHi(i) = 0
                    Next
                End If

                If (_cycle >= 2 AndAlso _cycle < 258) OrElse (_cycle >= 321 AndAlso _cycle < 338) Then
                    UpdateShifters()

                    Select Case ((_cycle - 1) Mod 8)
                        Case 0
                            LoadBackgroundShifters()
                            _bgNextTileId = PpuRead(&H2000US Or (_vramAddr.Reg And &HFFFUS))

                        Case 2
                            _bgNextTileAttrib = PpuRead(&H23C0US Or
                                                       (If(_vramAddr.NametableY, 1, 0) << 11) Or
                                                       (If(_vramAddr.NametableX, 1, 0) << 10) Or
                                                       ((_vramAddr.CoarseY >> 2) << 3) Or
                                                       (_vramAddr.CoarseX >> 2))

                            Dim shift As Byte = 0
                            If (_vramAddr.CoarseY And &H2) <> 0 Then shift += 4
                            If (_vramAddr.CoarseX And &H2) <> 0 Then shift += 2
                            _bgNextTileAttrib = (_bgNextTileAttrib >> shift) And &H3UI

                        Case 4
                            _bgNextTileLsb = PpuRead((If(_control.PatternBackground, &H1000US, 0US)) Or
                                                    (CUShort(_bgNextTileId) << 4) Or
                                                    _vramAddr.FineY)

                        Case 6
                            _bgNextTileMsb = PpuRead((If(_control.PatternBackground, &H1000US, 0US)) Or
                                                    (CUShort(_bgNextTileId) << 4) Or
                                                    _vramAddr.FineY + 8)

                        Case 7
                            IncrementScrollX()
                    End Select
                End If

                If _cycle = 256 Then IncrementScrollY()

                If _cycle = 257 Then
                    LoadBackgroundShifters()
                    TransferAddressX()
                End If

                If _cycle = 338 OrElse _cycle = 340 Then
                    _bgNextTileId = PpuRead(&H2000US Or (_vramAddr.Reg And &HFFFUS))
                End If

                If _scanline = -1 AndAlso _cycle >= 280 AndAlso _cycle < 305 Then
                    TransferAddressY()
                End If

                ' Sprite evaluation should happen at cycle 257 for the NEXT scanline
                If _cycle = 257 AndAlso _scanline >= 0 Then
                    EvaluateSprites()

                    ' DEBUG: Check OAM at scanline 100
                    'If _scanline = 100 Then
                    '    Console.WriteLine($"Scanline 100 - Sprite count: {_spriteCount}")
                    '    For i = 0 To Math.Min(3, 63)
                    '        Console.WriteLine($"OAM[{i}]: Y={OAM(i).Y}, Tile=${OAM(i).TileID:X2}, Attr=${OAM(i).Attributes:X2}, X={OAM(i).X}")
                    '    Next
                    'End If
                End If

                If _cycle = 340 Then
                    LoadSpriteShifters()
                End If

            End If

            ' Post-render scanline
            If _scanline = 240 Then
                ' Idle
            End If

            ' VBlank
            If _scanline >= 241 AndAlso _scanline < 261 Then
                If _scanline = 241 AndAlso _cycle = 1 Then
                    _status.VerticalBlank = True
                    If _control.EnableNmi Then NmiRequested = True
                End If
            End If

            ' Render pixel
            Dim bgPixel As Byte = 0, bgPalette As Byte = 0
            Dim fgPixel As Byte = 0, fgPalette As Byte = 0, fgPriority As Byte = 0

            If _mask.RenderBackground Then
                If _mask.RenderBackgroundLeft OrElse _cycle >= 9 Then
                    Dim mux = &H8000US >> _fineX
                    bgPixel = (If((_bgShifterPatternHi And mux) <> 0, 1, 0) << 1) Or If((_bgShifterPatternLo And mux) <> 0, 1, 0)
                    bgPalette = (If((_bgShifterAttribHi And mux) <> 0, 1, 0) << 1) Or If((_bgShifterAttribLo And mux) <> 0, 1, 0)
                End If
            End If

            If _mask.RenderSprites Then
                If _mask.RenderSpritesLeft OrElse _cycle >= 9 Then
                    _spriteZeroBeingRendered = False

                    For i = 0 To Math.Min(_spriteCount - 1, 7)
                        If _spriteScanline(i).X = 0 Then
                            fgPixel = (If((_spriteShifterHi(i) And &H80UI) <> 0, 1, 0) << 1) Or If((_spriteShifterLo(i) And &H80UI) <> 0, 1, 0)
                            fgPalette = (_spriteScanline(i).Attributes And &H3UI) + 4
                            fgPriority = If((_spriteScanline(i).Attributes And &H20UI) = 0, 1, 0)

                            If fgPixel <> 0 Then
                                If i = 0 Then _spriteZeroBeingRendered = True
                                Exit For
                            End If
                        End If
                    Next
                End If
            End If

            Dim pixel As Byte = 0, palette As Byte = 0

            If bgPixel = 0 AndAlso fgPixel = 0 Then
                pixel = 0 : palette = 0
            ElseIf bgPixel = 0 AndAlso fgPixel > 0 Then
                pixel = fgPixel : palette = fgPalette
            ElseIf bgPixel > 0 AndAlso fgPixel = 0 Then
                pixel = bgPixel : palette = bgPalette
            ElseIf bgPixel > 0 AndAlso fgPixel > 0 Then
                If fgPriority <> 0 Then
                    pixel = fgPixel : palette = fgPalette
                Else
                    pixel = bgPixel : palette = bgPalette
                End If

                If _spriteZeroHitPossible AndAlso _spriteZeroBeingRendered Then
                    If _mask.RenderBackground AndAlso _mask.RenderSprites Then
                        If Not (_mask.RenderBackgroundLeft Or _mask.RenderSpritesLeft) Then
                            If _cycle >= 9 AndAlso _cycle < 258 Then _status.SpriteZeroHit = True
                        Else
                            If _cycle >= 1 AndAlso _cycle < 258 Then _status.SpriteZeroHit = True
                        End If
                    End If
                End If
            End If

            If _scanline >= 0 AndAlso _scanline < 240 AndAlso _cycle >= 1 AndAlso _cycle < 257 Then
                _screen.SetPixel(_cycle - 1, _scanline, GetColorFromPalette(palette, pixel))
            End If

            _cycle += 1

            If _mask.RenderBackground OrElse _mask.RenderSprites Then
                If _cycle = 260 AndAlso _scanline < 240 Then
                    Cart.GetMapper.ScanlineCounter()
                End If
            End If

            If _cycle >= 341 Then
                _cycle = 0
                _scanline += 1
                If _scanline >= 261 Then
                    _scanline = -1
                    _frameComplete = True
                    _oddFrame = Not _oddFrame
                End If
            End If
        End Sub
#End Region

#Region "Debug Helpers"
        Public Function GetPatternTable(i As Byte, palette As Byte) As GraphicsObjects.Sprite
            For tileY = 0 To 15
                For tileX = 0 To 15
                    Dim offset = (tileY * 256) + (tileX * 16)

                    For row = 0 To 7
                        Dim tileLsb = PpuRead((CUShort(i) * &H1000US) + offset + row)
                        Dim tileMsb = PpuRead((CUShort(i) * &H1000US) + offset + row + 8)

                        For col = 0 To 7
                            Dim pixel = ((tileMsb And 1) << 1) Or (tileLsb And 1)
                            tileLsb >>= 1
                            tileMsb >>= 1

                            _patternTableSprites(i).SetPixel(tileX * 8 + (7 - col), tileY * 8 + row, GetColorFromPalette(palette, pixel))
                        Next
                    Next
                Next
            Next

            Return _patternTableSprites(i)
        End Function
#End Region

    End Class

End Namespace