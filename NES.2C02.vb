

Namespace NintendoEntertainmentSystem

    Public Class em2C02

        Public tblName(1, 1024) As Byte 'VB
        Private tblPalette(32) As Byte 'VB
        Private tblPattern(1, 4096) As Byte 'VB

        Public palScreen(&H40UI) As GraphicsObjects.Pixel
        Private sprScreen As New GraphicsObjects.Sprite(256, 240) 'Screen
        Private sprNameTable(1) As GraphicsObjects.Sprite         'Set the values in the constructor 'VB
        Private sprPatternTable(1) As GraphicsObjects.Sprite      'Set the values in the constructor 'VB

        ' Pixel offset horizont
        Private fine_x As Byte = &H0UI

        ' Internal communications
        Private address_latch As Byte = &H0UI
        Private ppu_data_buffer As Byte = &H0UI

        ' Pixel "dot" position information
        Private scanline As Int16 = 0
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

        Private FrameNumber As Integer = 0

        Public ReadOnly Property Debug_Scanline As Int16
            Get
                Return scanline
            End Get
        End Property

        ' Expose control/mask/status as read-only bytes for diagnostics
        Public ReadOnly Property Debug_PPUControlReg As Byte
            Get
                Return CByte(PPUControl.Reg And &HFF)
            End Get
        End Property

        Public ReadOnly Property Debug_PPUMaskReg As Byte
            Get
                Return CByte(PPUMask.Reg And &HFF)
            End Get
        End Property

        Public ReadOnly Property Debug_PPUStatusReg As Byte
            Get
                Return CByte(PPUStatus.Reg And &HFF)
            End Get
        End Property

        ' Expose loopy regs
        Public ReadOnly Property Debug_VramReg As UInt16
            Get
                Return vram_addr.Reg
            End Get
        End Property

        Public ReadOnly Property Debug_TramReg As UInt16
            Get
                Return tram_addr.Reg
            End Get
        End Property

        ' Return a copy of the palette RAM bytes
        Public Function Debug_GetTblPalette() As Byte()
            Dim out(tblPalette.Length - 1) As Byte
            Array.Copy(tblPalette, out, tblPalette.Length)
            Return out
        End Function

        ' Return a contiguous slice of a nametable row (rowIndex: 0 or 1)
        Public Function Debug_GetNameTableRow(rowIndex As Integer, startIndex As Integer, count As Integer) As Byte()
            Dim out(Math.Max(0, count - 1)) As Byte
            For i As Integer = 0 To count - 1
                out(i) = tblName(rowIndex, startIndex + i)
            Next
            Return out
        End Function

        ' Return pattern memory slice from specified pattern table (0 or 1)
        Public Function Debug_GetPatternBytes(tableIndex As Integer, startIndex As Integer, count As Integer) As Byte()
            Dim out(Math.Max(0, count - 1)) As Byte
            For i As Integer = 0 To count - 1
                out(i) = tblPattern(tableIndex, startIndex + i)
            Next
            Return out
        End Function

        ' Temporarily enable rendering (write mask). Use only for debugging.
        Public Sub Debug_SetPPUMask(mask As Byte)
            PPUMask.Reg = mask
        End Sub

        ' Fill both nametables with a single tile id for quick background test.
        Public Sub Debug_FillNameTables(tileId As Byte)
            For i As Integer = 0 To 1023
                tblName(0, i) = tileId
                tblName(1, i) = tileId
            Next
        End Sub

        ' Populate palette RAM with sequential entries (0..31) for a visible palette.
        Public Sub Debug_FillPaletteSequential()
            For i As Integer = 0 To Math.Min(tblPalette.Length - 1, 31)
                tblPalette(i) = CByte(i And &H3F)
            Next
        End Sub

        Public OAM(63) As OAMEntry
        '[OAM[address][address]=OAM(address \ 4).G/SetByteAt(address)] 'Math.Floor(addr / 4)
        'Suppose could have just made this an array of 256 bytes heh

        '// A register to store the address when the CPU manually communicates
        '// with OAM via PPU registers. This Is Not commonly used because it 
        '// Is very slow, And instead a 256-Byte DMA transfer Is used. See
        '// the Bus header for a description of this.
        Private oam_addr As Byte = 0

        Private spriteScanline(7) As OAMEntry 'VB 8
        Private sprite_count As Byte = 0
        Private sprite_shifter_pattern_lo(7) As Byte 'VB 8
        Private sprite_shifter_pattern_hi(7) As Byte 'VB 8

        ' Sprite zero collision flags
        Private bSpriteZeroHitPossible As Boolean = False
        Private bSpriteZeroBeingRendered As Boolean = False

        Public frame_complete As Boolean = False

        Private PPUStatus As New PpuStatusRegister
        Private PPUMask As New PpuMaskRegister
        Private PPUControl As New PpuControlRegister
        Protected vram_addr As New LoopyRegister
        Protected tram_addr As New LoopyRegister

        Private attr_shift_table(3, 3) As Byte

        Public Sub New()
            sprNameTable = {New GraphicsObjects.Sprite(256, 240), New GraphicsObjects.Sprite(256, 240)}
            sprPatternTable = {New GraphicsObjects.Sprite(128, 128), New GraphicsObjects.Sprite(128, 128)}

            'Public tblName(1, 1024) As Byte 'VB
            'Private tblPalette(32) As Byte 'VB
            'Private tblPattern(1, 4096) As Byte 'VB
            For i As Integer = 0 To 4095
                If i < 32 Then
                    tblPalette(i) = &H0
                End If
                If i < 1024 Then
                    tblName(0, i) = &H0
                    tblName(1, i) = &H0
                End If
                If i < 4096 Then
                    tblPattern(0, i) = &H0
                    tblPattern(1, i) = &H0
                End If
            Next

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

            ' Initialize attribute shift lookup table
            ' [y][x] where y and x are 0 or 1 (from coarse_y & 2 and coarse_x & 2)
            attr_shift_table(0, 0) = 0  ' Top-left: bits 1-0
            attr_shift_table(0, 1) = 2  ' Top-right: bits 3-2
            attr_shift_table(1, 0) = 4  ' Bottom-left: bits 5-4
            attr_shift_table(1, 1) = 6  ' Bottom-right: bits 7-6
        End Sub

        'Public Sub DiagnoseAttributeTable()
        '    Debug.WriteLine("=== Attribute Table Diagnostic ===")
        '
        '    ' Read first 64 bytes of attribute table for nametable 0
        '    Debug.WriteLine("First 64 attribute bytes at $23C0:")
        '    For i As Integer = 0 To 63
        '        Dim addr As UInt16 = &H23C0US + CUShort(i)
        '        Dim value As Byte = ppuRead(addr)
        '        Debug.Write(String.Format("{0:X2} ", value))
        '        If (i + 1) Mod 8 = 0 Then Debug.WriteLine("")
        '    Next
        '
        '    Debug.WriteLine("")
        '    Debug.WriteLine("First 32 nametable tiles:")
        '    For i As Integer = 0 To 31
        '        Dim tile As Byte = ppuRead(&H2000US + CUShort(i))
        '        Debug.Write(String.Format("{0:X2} ", tile))
        '        If (i + 1) Mod 8 = 0 Then Debug.WriteLine("")
        '    Next
        '
        '    Debug.WriteLine("=== End Diagnostic ===")
        'End Sub

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
                    'Dim nOffset As UInt16 = MathHelpers.SafeAddition16(MathHelpers.SafeMul16(nTileY, 256), MathHelpers.SafeMul16(nTileX, 16))
                    Dim nOffset As UInt16 = ((nTileY * 256) + (nTileX * 16))

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
                            Dim pixel As Byte = ((tile_msb And &H1UI) << 1) Or (tile_lsb And &H1UI)

                            '// ...we will shift the row words 1 bit right for each column of
                            '// the character.
                            tile_lsb >>= 1
                            tile_msb >>= 1

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
                    Case &H0US : bytData = PPUControl.Reg : Exit Select ' Control
                    Case &H1US : bytData = PPUMask.Reg : Exit Select    ' Mask
                    Case &H2US : bytData = PPUStatus.Reg : Exit Select  ' Status
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
                        bytData = (PPUStatus.Reg And &HE0UI) Or (ppu_data_buffer And &H1FUI)
                        '// Clear the vertical blanking flag
                        PPUStatus.VerticalBlank = False
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
                        If PPUControl.IncrementMode Then
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
                Case &H0US  ' $2000 - PPUCTRL
                    Dim oldControl As Byte = PPUControl.Reg
                    PPUControl.Reg = data
                    tram_addr.NametableX = PPUControl.NametableX
                    tram_addr.NametableY = PPUControl.NametableY

                    ' LOG EVERY WRITE TO $2000
                    'Debug.WriteLine(String.Format("[Frame {0}] $2000 write: ${1:X2} (was ${2:X2}) - NMI: {3}→{4}, PC=${5:X4}",
                    '              frameCount,
                    '              data,
                    '              oldControl,
                    '              (oldControl And &H80) >> 7,
                    '              (data And &H80) >> 7,
                    '              CPU.Debug_PC))
                    Exit Select

                Case &H1US  ' $2001 - PPUMASK
                    PPUMask.Reg = data
                    'Debug.WriteLine(String.Format("[SCANLINE {0:D3} CYCLE {1:D3}] PPU $2001 write: 0x{2:X2} (BG={3}, SPR={4})",
                    '                      scanline, cycle, data,
                    '                      PPUMask.Render_background, PPUMask.Render_sprites))
                    Exit Select

                Case &H2US  ' $2002 - PPUSTATUS (not writable)
                    Exit Select

                Case &H3US  ' $2003 - OAMADDR
                    oam_addr = data
                    Exit Select

                Case &H4US  ' $2004 - OAMDATA
                    OAM(oam_addr \ 4).SetByteAt(oam_addr, data)
                    Exit Select

                Case &H5US  ' $2005 - PPUSCROLL *** THIS IS THE CRITICAL ONE ***
                    If address_latch = 0 Then
                        fine_x = data And &H7UI
                        tram_addr.CoarseX = data >> 3
                        address_latch = 1
                        'Debug.WriteLine(String.Format("[SCANLINE {0:D3} CYCLE {1:D3}] *** SCROLL X ***: data=0x{2:X2}, fine_x={3}, coarse_x={4}",
                        '                      scanline, cycle, data, fine_x, tram_addr.Coarse_X))
                    Else
                        tram_addr.FineY = data And &H7UI
                        tram_addr.CoarseY = data >> 3
                        address_latch = 0
                        'Debug.WriteLine(String.Format("[SCANLINE {0:D3} CYCLE {1:D3}] *** SCROLL Y ***: data=0x{2:X2}, fine_y={3}, coarse_y={4}",
                        '                      scanline, cycle, data, tram_addr.Fine_Y, tram_addr.Coarse_Y))
                    End If
                    Exit Select

                Case &H6US  ' $2006 - PPUADDR
                    If address_latch = 0 Then
                        tram_addr.Reg = MathHelpers.SafeShiftLeft16((data And &H3FUS), 8) Or (tram_addr.Reg And &HFFUS)
                        address_latch = 1
                    Else
                        tram_addr.Reg = (tram_addr.Reg And &HFF00US) Or data
                        vram_addr.Reg = tram_addr.Reg
                        address_latch = 0
                    End If
                    'Debug.WriteLine(String.Format("[SCANLINE {0:D3} CYCLE {1:D3}] PPU $2006 write: vram_addr=0x{2:X4}, tram_addr=0x{3:X4}",
                    '                      scanline, cycle, vram_addr.Reg, tram_addr.Reg))
                    Exit Select

                Case &H7US  ' $2007 - PPUDATA
                    'Debug.WriteLine(String.Format("[SCANLINE {0:D3} CYCLE {1:D3}] PPU $2007 write: addr=0x{2:X4}, data=0x{3:X2}",
                    '                      scanline, cycle, vram_addr.Reg, data))
                    ppuWrite(vram_addr.Reg, data)
                    If PPUControl.IncrementMode Then
                        vram_addr.Reg = vram_addr.Reg + 32
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
                If Cart.MirrorMode() = [MirrorMode].Vertical Then
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
                ElseIf Cart.MirrorMode() = [MirrorMode].HORIZONTAL Then
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
                If PPUMask.Grayscale Then
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
                If Cart.MirrorMode() = [MirrorMode].Vertical Then
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
                ElseIf Cart.MirrorMode() = [MirrorMode].HORIZONTAL Then
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
                ' Palette mirroring
                Select Case addr
                    Case &H10US : addr = &H0US
                    Case &H14US : addr = &H4US
                    Case &H18US : addr = &H8US
                    Case &H1CUS : addr = &HCUS
                End Select
                tblPalette(addr) = data
                'Debug.WriteLine(String.Format("Palette write: $3F{0:X2} = 0x{1:X2} (color index {2})",
                '                      addr, data, data And &H3F))
            End If
        End Sub

        Public Sub Reset()
            For i As Integer = 0 To 63
                OAM(i).Fill(&HFFUI)
            Next
            oam_addr = 0
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
            PPUStatus.Reg = 0
            PPUMask.Reg = 0
            PPUControl.Reg = 0
            vram_addr.Reg = 0
            tram_addr.Reg = 0
            scanline_trigger = False
            odd_frame = False
        End Sub

        'Public Sub DebugScrollState()
        '    Debug.WriteLine("=".PadRight(80, "="))
        '    Debug.WriteLine(String.Format("NEW FRAME START (scanline={0}, cycle={1})", scanline, cycle))
        '    Debug.WriteLine(String.Format("  vram_addr: 0x{0:X4} (Coarse_X={1:D2}, Coarse_Y={2:D2}, Fine_Y={3}, NT_X={4}, NT_Y={5})",
        '                          vram_addr.Reg, vram_addr.Coarse_X, vram_addr.Coarse_Y,
        '                          vram_addr.Fine_Y, vram_addr.NameTable_X, vram_addr.NameTable_Y))
        '    Debug.WriteLine(String.Format("  tram_addr: 0x{0:X4} (Coarse_X={1:D2}, Coarse_Y={2:D2}, Fine_Y={3}, NT_X={4}, NT_Y={5})",
        '                          tram_addr.Reg, tram_addr.Coarse_X, tram_addr.Coarse_Y,
        '                          tram_addr.Fine_Y, tram_addr.NameTable_X, tram_addr.NameTable_Y))
        '    Debug.WriteLine(String.Format("  fine_x: {0}", fine_x))
        '    Debug.WriteLine(String.Format("  PPUControl: 0x{0:X2} (NMI={1}, BG_Pattern={2})",
        '                          PPUControl.Reg, PPUControl.Enable_nmi, PPUControl.Pattern_background))
        '    Debug.WriteLine(String.Format("  PPUMask: 0x{0:X2} (Render_BG={1}, Render_SPR={2})",
        '                          PPUMask.Reg, PPUMask.Render_background, PPUMask.Render_sprites))
        '    Debug.WriteLine("=".PadRight(80, "="))
        'End Sub

        Public Sub ClockOld()

            Dim IncrementScrollX = Sub()
                                       If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                                           If vram_addr.CoarseX = 31 Then
                                               vram_addr.CoarseX = 0
                                               vram_addr.NametableX = Not vram_addr.NametableX
                                           Else
                                               vram_addr.CoarseX += 1
                                           End If
                                       End If
                                   End Sub
            Dim IncrementScrollY = Sub()
                                       ' ONLY increment if rendering is enabled
                                       If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                                           If vram_addr.FineY < 7 Then
                                               vram_addr.FineY += 1
                                           Else
                                               vram_addr.FineY = 0
                                               If vram_addr.CoarseY = 29 Then
                                                   vram_addr.CoarseY = 0
                                                   vram_addr.NametableY = Not vram_addr.NametableY
                                               ElseIf vram_addr.CoarseY = 31 Then
                                                   vram_addr.CoarseY = 0
                                               Else
                                                   vram_addr.CoarseY += 1
                                               End If
                                           End If
                                       End If
                                   End Sub
            Dim TransferAddressX = Sub()
                                       ' Only transfer during rendering
                                       If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                                           vram_addr.NametableX = tram_addr.NametableX
                                           vram_addr.CoarseX = tram_addr.CoarseX
                                       End If
                                   End Sub
            Dim TransferAddressY = Sub()
                                       ' Only transfer during rendering
                                       If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                                           vram_addr.FineY = tram_addr.FineY
                                           vram_addr.NametableY = tram_addr.NametableY
                                           vram_addr.CoarseY = tram_addr.CoarseY
                                       End If
                                   End Sub
            Dim LoadBackgroundShifters = Sub()
                                             ' Load pattern shifters with next tile data
                                             bg_shifter_pattern_lo = (bg_shifter_pattern_lo And &HFF00US) Or CUShort(bg_next_tile_lsb)
                                             bg_shifter_pattern_hi = (bg_shifter_pattern_hi And &HFF00US) Or CUShort(bg_next_tile_msb)

                                             ' Load attribute shifters
                                             ' If bit 0 of attribute is set, load 0xFF (all bits on), else 0x00 (all bits off)
                                             ' This expands the single bit across all 8 pixels of the tile
                                             If (bg_next_tile_attrib And &H1) <> 0 Then
                                                 bg_shifter_attrib_lo = (bg_shifter_attrib_lo And &HFF00US) Or &HFFUS
                                             Else
                                                 bg_shifter_attrib_lo = (bg_shifter_attrib_lo And &HFF00US)
                                             End If

                                             If (bg_next_tile_attrib And &H2) <> 0 Then
                                                 bg_shifter_attrib_hi = (bg_shifter_attrib_hi And &HFF00US) Or &HFFUS
                                             Else
                                                 bg_shifter_attrib_hi = (bg_shifter_attrib_hi And &HFF00US)
                                             End If
                                         End Sub
            Dim UpdateShifters = Sub()
                                     If PPUMask.RenderBackground Then
                                         bg_shifter_pattern_lo <<= 1
                                         bg_shifter_pattern_hi <<= 1

                                         bg_shifter_attrib_lo <<= 1
                                         bg_shifter_attrib_hi <<= 1
                                     End If
                                     If PPUMask.RenderSprites AndAlso cycle >= 1 AndAlso cycle < 258 Then
                                         If sprite_count > 0 Then
                                             For i As Byte = 0 To (sprite_count - 1)
                                                 If spriteScanline(i).X > 0 Then
                                                     spriteScanline(i).X = spriteScanline(i).X - 1
                                                 Else
                                                     sprite_shifter_pattern_lo(i) <<= 1
                                                     sprite_shifter_pattern_hi(i) <<= 1
                                                 End If
                                             Next
                                         End If
                                     End If
                                 End Sub

            If scanline >= -1 AndAlso scanline < 240 Then
                ' Background Rendering
                If scanline = 0 AndAlso cycle = 0 AndAlso odd_frame AndAlso (PPUMask.RenderBackground OrElse PPUMask.RenderSprites) Then
                    ' Odd frame, cycle skip
                    cycle = 1
                End If
                '-----------------------------
                If scanline = -1 AndAlso cycle = 1 Then
                    '' TEMPORARY HACK: Force NMI enable
                    'If PPUControl.Reg = &H11 Then
                    '    Debug.WriteLine("!!! FORCING NMI ENABLE (changing $11 to $91) !!!")
                    '    PPUControl.Reg = &H91
                    'End If

                    FrameNumber += 1
                    'Debug.WriteLine(String.Format("*** FRAME {0} ***", FrameNumber))
                    'DebugScrollState()

                    '// Effectively start of new frame, so clear vertical blank flag
                    PPUStatus.VerticalBlank = False
                    '// Clear sprite overflow flag
                    PPUStatus.SpriteOverflow = False
                    '// Clear the sprite zero hit flag
                    PPUStatus.SpriteZeroHit = False
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
                            ' Fetch the next background tile attribute
                            ' 
                            ' Attribute memory structure:
                            ' - Starts at $23C0 (or +$0400 for nametable 1, +$0800 for NT 2, +$0C00 for NT 3)
                            ' - Each byte controls a 4x4 tile region (32x32 pixels, or 4x4 tiles)
                            ' - Each 4x4 region is divided into four 2x2 tile quadrants
                            ' - Each quadrant gets 2 bits (4 possible palettes: 0-3)
                            '
                            ' Byte layout: [BR][BL][TR][TL] where T=Top, B=Bottom, L=Left, R=Right
                            '              bits: 7-6, 5-4, 3-2, 1-0

                            ' Calculate attribute table address
                            ' The attribute table is 8x8 bytes (covering 32x32 tiles = 256x240 pixels)
                            bg_next_tile_attrib = ppuRead(&H23C0US Or
                              (If(vram_addr.NametableY, 1, 0) << 11) Or
                              (If(vram_addr.NametableX, 1, 0) << 10) Or
                              ((vram_addr.CoarseY >> 2) << 3) Or
                              (vram_addr.CoarseX >> 2))

                            ' Now extract the correct 2 bits based on which quadrant we're in
                            ' within the 4x4 tile region
                            '
                            ' Quadrant determination:
                            ' - Bit 1 of Coarse_Y determines top (0) or bottom (1) half
                            ' - Bit 1 of Coarse_X determines left (0) or right (1) half
                            '
                            ' Truth table:
                            ' Coarse_Y[1] | Coarse_X[1] | Quadrant | Bits | Shift
                            ' ------------|-------------|----------|------|-------
                            '      0      |      0      | Top-Left |  1-0 |   0
                            '      0      |      1      | Top-Right|  3-2 |   2
                            '      1      |      0      | Bot-Left |  5-4 |   4
                            '      1      |      1      | Bot-Right|  7-6 |   6

                            ' Method 1: Using conditional shifts (CLEAREST)
                            Dim y_bit As Integer = If((vram_addr.CoarseY And &H2US) <> 0, 1, 0)
                            Dim x_bit As Integer = If((vram_addr.CoarseX And &H2US) <> 0, 1, 0)
                            Dim shift_amount As Byte = attr_shift_table(y_bit, x_bit)

                            bg_next_tile_attrib = (bg_next_tile_attrib >> shift_amount) And &H3UI

                            ' DEBUG: Log first few attributes on scanline 0
                            'Static debugCount As Integer = 0
                            'If scanline = 0 AndAlso cycle < 100 AndAlso (cycle - 2) Mod 8 = 0 Then
                            '    Debug.WriteLine(String.Format("Tile pos ({0},{1}), attr byte=0x{2:X2}, extracted={3}, " &
                            '          "coarse_y[1]={4}, coarse_x[1]={5}",
                            '          vram_addr.Coarse_X, vram_addr.Coarse_Y,
                            '          ppuRead(&H23C0US Or (vram_addr.NameTable_Y << 11) Or
                            '                 (vram_addr.NameTable_X << 10) Or
                            '                 ((vram_addr.Coarse_Y >> 2) << 3) Or
                            '                 (vram_addr.Coarse_X >> 2)),
                            '          bg_next_tile_attrib,
                            '          (vram_addr.Coarse_Y And &H2US) >> 1,
                            '          (vram_addr.Coarse_X And &H2US) >> 1))
                            '    debugCount += 1
                            '    If debugCount > 10 Then debugCount = 0  ' Reset to avoid spam
                            'End If

                            Exit Select                            '// Compared to the last two, the next two are the easy ones... :P
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
                            bg_next_tile_lsb = ppuRead(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(MathHelpers.SafeShiftLeft16(If(PPUControl.PatternBackground, 1, 0), 12), MathHelpers.SafeShiftLeft16(bg_next_tile_id, 4)), vram_addr.FineY + 0))
                            Exit Select
                        Case 6
                            '// Fetch the next background tile MSB bit plane from the pattern memory
                            '// This Is the same as above, but has a +8 offset to select the next bit plane
                            'bg_next_tile_msb = ppuRead((PPUControl.pattern_background << 12) + (bg_next_tile_id << 4) + (vram_addr.Fine_Y) + 8) 'VBMATH checkme
                            bg_next_tile_msb = ppuRead(MathHelpers.SafeAddition16(MathHelpers.SafeAddition16(MathHelpers.SafeShiftLeft16(If(PPUControl.PatternBackground, 1, 0), 12), MathHelpers.SafeShiftLeft16(bg_next_tile_id, 4)), MathHelpers.SafeAddition16((vram_addr.FineY), 8))) 'VBMATH checkme
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
                ' In the Clock() method, around scanline -1, cycle 280-304:
                If scanline = -1 AndAlso cycle >= 280 AndAlso cycle < 305 Then
                    '// End of vertical blank period so reset the Y address ready for rendering
                    '-----------------------------
                    'Temp force scrolls
                    '-----------------------------
                    'If cycle = 280 Then
                    '    Debug.WriteLine("!!! FORCING SCROLL TO 0,0 (improved) !!!")
                    '    tram_addr.Coarse_X = 0
                    '    tram_addr.Coarse_Y = 0
                    '    tram_addr.Fine_Y = 0
                    '    tram_addr.NameTable_X = 0
                    '    tram_addr.NameTable_Y = 0
                    '    fine_x = 0
                    'End If
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
                    For i As UInt32 = 0 To spriteScanline.Length() - 1 'VB [std::memset(spriteScanline, 0xFF, 8 * sizeof(sObjectAttributeEntry));]
                        spriteScanline(i).Fill(&HFFUI) '&H7FUI) why did i use 7F again?
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
                    While ((nOAMEntry < 64) AndAlso (sprite_count < 9))
                        '// Note the conversion to signed numbers here
                        Dim diff As Int16 = scanline - OAM(nOAMEntry).Y

                        '// If the difference Is positive then the scanline Is at least at the
                        '// same height as the sprite, so check if it resides in the sprite vertically
                        '// depending on the current "sprite height mode"
                        '// FLAGGED

                        'Dim iif_var As Integer = IIf(PPUControl.sprite_size, 16, 8) 'was checking
                        If diff >= 0 AndAlso diff < IIf(PPUControl.SpriteSize, 16, 8) AndAlso sprite_count < 8 Then
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
                                spriteScanline(sprite_count).CopyFrom(OAM(nOAMEntry))
                            End If
                            sprite_count += 1
                        End If
                        nOAMEntry += 1
                    End While '// End of sprite evaluation for next scanline

                    '// Set sprite overflow flag
                    PPUStatus.SpriteOverflow = (sprite_count >= 8)

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

                    If sprite_count > 0 Then 'Because if left as 0 the sprite count will throw an over flow when sprite_count = 0 (VB For Loops Problem not being able to if i < value)
                        For i As Byte = 0 To sprite_count - 1 'VB because vb has no for i = 0; i < spritecount; i++;
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
                            If Not PPUControl.SpriteSize Then
                                '// 8x8 Sprite Mode - The control register determines the pattern table
                                If Not (spriteScanline(i).Attributes And &H80UI) Then
                                    '// Sprite is NOT flipped vertically, i.e. normal    
                                    sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(If(PPUControl.PatternSprite, 1, 0), 12) Or  '// Which Pattern Table? 0KB or 4KB offset
                                                     MathHelpers.SafeShiftLeft16(spriteScanline(i).TileID, 4) Or        '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                     ((scanline - spriteScanline(i).Y) And 7)                '// Which Row in cell? (0->7) [shouldent there be an & 0x7 here]
                                Else
                                    '// Sprite is flipped vertically, i.e. upside down
                                    sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(If(PPUControl.PatternSprite, 1, 0), 12) Or                  '// Which Pattern Table? 0KB or 4KB offset
                                                     MathHelpers.SafeShiftLeft16(spriteScanline(i).TileID, 4) Or                        '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                     MathHelpers.SafeSubtract16(7, ((scanline - spriteScanline(i).Y) And 7)) '// Which Row in cell? (7->0)
                                End If
                            Else
                                '// 8x16 Sprite Mode - The sprite attribute determines the pattern table
                                If Not (spriteScanline(i).Attributes And &H80UI) Then
                                    '// Sprite is NOT flipped vertically, i.e. normal
                                    If (scanline - spriteScanline(i).Y) < 8 Then
                                        '// Reading Top half Tile
                                        sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16((spriteScanline(i).TileID And &H1UI), 12) Or   '// Which Pattern Table? 0KB or 4KB offset
                                                         MathHelpers.SafeShiftLeft16((spriteScanline(i).TileID And &HFEUI), 4) Or   '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                         ((scanline - spriteScanline(i).Y) And &H7UI)                           '// Which Row in cell? (0->7)
                                    Else
                                        '// Reading Bottom Half Tile
                                        sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16((spriteScanline(i).TileID And &H1UI), 12) Or                                   '// Which Pattern Table? 0KB or 4KB offset
                                                         MathHelpers.SafeShiftLeft16(MathHelpers.SafeAddition16((spriteScanline(i).TileID And &HFEUI), 1), 4) Or    '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                         ((scanline - spriteScanline(i).Y) And &H7UI)                                                           '// Which Row in cell? (0->7)
                                    End If
                                Else
                                    '// Sprite is flipped vertically, i.e. upside down
                                    If (scanline - spriteScanline(i).y < 8) Then
                                        sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16((spriteScanline(i).TileID And &H1UI), 12) Or                                   '// Which Pattern Table? 0KB or 4KB offset
                                                         MathHelpers.SafeShiftLeft16(MathHelpers.SafeAddition16((spriteScanline(i).TileID And &HFEUI), 1), 4) Or    '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                         MathHelpers.SafeSubtract16(7, (scanline - spriteScanline(i).Y) And &H7UI)                              '// Which Row in cell? (0->7)

                                    Else
                                        sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16((spriteScanline(i).TileID And &H1UI), 12) Or       '// Which Pattern Table? 0KB or 4KB offset
                                                         MathHelpers.SafeShiftLeft16((spriteScanline(i).TileID And &HFEUI), 4) Or       '// Which Cell? Tile ID * 16 (16 bytes per tile)
                                                         MathHelpers.SafeSubtract16(7, (scanline - spriteScanline(i).Y) And &H7UI)  '// Which Row in cell? (0->7)
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
                            If spriteScanline(i).Attributes And &H40UI Then
                                '// This little lambda function "flips" a byte
                                '// so 0b11100000 becomes 0b00000111. It's very
                                '// clever, And stolen completely from here:
                                '// https//stackoverflow.com/a/2602885
                                Dim flipbyte = Function(ByVal b1 As Byte) As Byte
                                                   If b1 = 0 Then Return 0
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
                    PPUStatus.VerticalBlank = 1

                    '// If the control register tells us to emit a NMI when
                    '// entering vertical blanking period, do it! The CPU
                    '// will be informed that rendering Is complete so it can
                    '// perform operations with the PPU knowing it wont
                    '// produce visible artefacts
                    If PPUControl.EnableNmi Then nmi = True
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
            If PPUMask.RenderBackground Then
                If PPUMask.RenderBackgroundLeft OrElse (cycle >= 9) Then
                    Dim bit_mux As UInt16 = &H8000US >> fine_x

                    ' Extract 2-bit pixel value
                    Dim p0_pixel As Byte = If((bg_shifter_pattern_lo And bit_mux) <> 0, 1, 0)
                    Dim p1_pixel As Byte = If((bg_shifter_pattern_hi And bit_mux) <> 0, 1, 0)
                    bg_pixel = (p1_pixel << 1) Or p0_pixel

                    ' Extract 2-bit palette value
                    Dim bg_pal0 As Byte = If((bg_shifter_attrib_lo And bit_mux) <> 0, 1, 0)
                    Dim bg_pal1 As Byte = If((bg_shifter_attrib_hi And bit_mux) <> 0, 1, 0)
                    bg_palette = (bg_pal1 << 1) Or bg_pal0
                End If
            End If
            '-----------------------------
            '// Foreground =============================================================
            '-----------------------------
            Dim fg_pixel As Byte = 0    '// The 2-bit pixel to be rendered
            Dim fg_palette As Byte = 0  '// The 3-bit index of the palette the pixel indexes
            Dim fg_priority As Byte = 0 '// A bit of the sprite attribute indicates if its
            '                           '// more important than the background

            If PPUMask.RenderSprites Then
                '// Iterate through all sprites for this scanline. This Is to maintain
                '// sprite priority. As soon as we find a non transparent pixel of
                '// a sprite we can abort
                'If PPUMask.Render_sprites_left OrElse (cycle >= 9) Then
                '
                '    bSpriteZeroBeingRendered = False
                '
                '    If sprite_count > 0 Then 'same reason as further up the chain sprite_count = 0 = overflow in vb (the c++ counterpart just skips this vb does not)
                '        For i As Byte = 0 To (sprite_count - 1) 'VB
                '            '// Scanline cycle has "collided" with sprite, shifters taking over
                '            If spriteScanline(i).x = 0 Then
                '                '// Note Fine X scrolling does Not apply to sprites, the game
                '                '// should maintain their relationship with the background. So
                '                '// we'll just use the MSB of the shifter
                '
                '                '// Determine the pixel value...
                '                'Dim fg_pixel_lo As Byte = (sprite_shifter_pattern_lo(i) And &H80UI) > 0
                '                'Dim fg_pixel_hi As Byte = (sprite_shifter_pattern_hi(i) And &H80UI) > 0
                '                'fg_pixel = MathHelpers.SafeShiftLeft8(fg_pixel_hi, 1) Or fg_pixel_lo
                '                Dim fg_pixel_lo As Byte = If((sprite_shifter_pattern_lo(i) And &H80UI) <> 0, 1, 0)
                '                Dim fg_pixel_hi As Byte = If((sprite_shifter_pattern_hi(i) And &H80UI) <> 0, 1, 0)
                '                fg_pixel = (fg_pixel_hi << 1) Or fg_pixel_lo
                '
                '                '// Extract the palette from the bottom two bits. Recall
                '                '// that foreground palettes are the latter 4 in the 
                '                '// palette memory.
                '                'fg_palette = (spriteScanline(i).attribute And &H3UI) + &H4UI
                '                'fg_priority = ((spriteScanline(i).attribute And &H20UI) = 0)
                '                fg_palette = (spriteScanline(i).attribute And &H3UI) + &H4UI
                '                fg_priority = If((spriteScanline(i).attribute And &H20UI) = 0, 1, 0)
                '
                '                '// If pixel Is Not transparent, we render it, And dont
                '                '// bother checking the rest because the earlier sprites
                '                '// in the list are higher priority
                '                If fg_pixel <> 0 Then
                '                    If i = 0 Then '// Is this sprite zero?
                '                        bSpriteZeroBeingRendered = True
                '                    End If
                '                    Exit For 'break;
                '                End If
                '                '-----------------------------
                '            End If
                '        Next
                '    End If
                'End If

                If PPUMask.RenderSprites Then
                    If PPUMask.RenderSpritesLeft OrElse (cycle >= 9) Then
                        bSpriteZeroBeingRendered = False

                        If sprite_count > 0 Then
                            For i As Byte = 0 To (sprite_count - 1)
                                If spriteScanline(i).X = 0 Then
                                    Dim fg_pixel_lo As Byte = If((sprite_shifter_pattern_lo(i) And &H80UI) <> 0, 1, 0)
                                    Dim fg_pixel_hi As Byte = If((sprite_shifter_pattern_hi(i) And &H80UI) <> 0, 1, 0)
                                    fg_pixel = (fg_pixel_hi << 1) Or fg_pixel_lo

                                    fg_palette = (spriteScanline(i).Attributes And &H3UI) + &H4UI
                                    fg_priority = If((spriteScanline(i).Attributes And &H20UI) = 0, 1, 0)

                                    If fg_pixel <> 0 Then
                                        If i = 0 Then
                                            bSpriteZeroBeingRendered = True
                                        End If
                                        Exit For
                                    End If
                                End If
                            Next
                        End If
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
                If fg_priority <> 0 Then
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
                    If PPUMask.RenderBackground And PPUMask.RenderSprites Then
                        '// The left edge of the screen has specific switches to control
                        '// its appearance. This Is used to smooth inconsistencies when
                        '// scrolling (since sprites x coord must be >= 0)
                        'If Not (PPUMask.Render_background_left Or PPUMask.Render_sprites_left) Then
                        '    If cycle >= 9 AndAlso cycle < 258 Then
                        '        PPUStatus.Sprite_zero_hit = 1
                        '    End If
                        'Else
                        '    If cycle >= 1 AndAlso cycle < 258 Then
                        '        PPUStatus.Sprite_zero_hit = 1
                        '    End If
                        'End If
                        'if (~(mask.render_background_left | mask.render_sprites_left))
                        If (PPUMask.RenderBackgroundLeft Or PPUMask.RenderSpritesLeft) = 0 Then
                            ' Neither flag set - use stricter range
                            If cycle >= 9 AndAlso cycle < 258 Then
                                PPUStatus.SpriteZeroHit = True
                            End If
                        Else
                            ' At least one flag set - use full range
                            If cycle >= 1 AndAlso cycle < 258 Then
                                PPUStatus.SpriteZeroHit = True
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
            If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                If cycle = 260 AndAlso scanline < 240 Then
                    Cart.GetMapper.ScanlineCounter()
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

            '---------------------
            'Static lastVramAddr As UInt16 = 0
            'Static lastScanline As Int16 = -2
            '
            ' Only log when vram changes and we're NOT in visible rendering area
            'If vram_addr.Reg <> lastVramAddr AndAlso Not (scanline >= 0 AndAlso scanline < 240 AndAlso cycle >= 1 AndAlso cycle < 257) Then
            '    Debug.WriteLine(String.Format("VRAM changed outside rendering: scanline={0}, cycle={1}, old=0x{2:X4}, new=0x{3:X4}",
            '                                  scanline, cycle, lastVramAddr, vram_addr.Reg))
            'End If
            '
            'lastVramAddr = vram_addr.Reg
            'lastScanline = scanline
        End Sub 'pretty sure the entire clock sub needs a look over

        Public Sub Clock()
            ' Helper lambdas for scroll operations
            Dim IncrementScrollX = Sub()
                                       If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                                           If vram_addr.CoarseX = 31 Then
                                               vram_addr.CoarseX = 0
                                               vram_addr.NametableX = Not vram_addr.NametableX
                                           Else
                                               vram_addr.CoarseX += 1
                                           End If
                                       End If
                                   End Sub

            Dim IncrementScrollY = Sub()
                                       If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                                           If vram_addr.FineY < 7 Then
                                               vram_addr.FineY += 1
                                           Else
                                               vram_addr.FineY = 0
                                               If vram_addr.CoarseY = 29 Then
                                                   vram_addr.CoarseY = 0
                                                   vram_addr.NametableY = Not vram_addr.NametableY
                                               ElseIf vram_addr.CoarseY = 31 Then
                                                   vram_addr.CoarseY = 0
                                               Else
                                                   vram_addr.CoarseY += 1
                                               End If
                                           End If
                                       End If
                                   End Sub

            Dim TransferAddressX = Sub()
                                       If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                                           vram_addr.NametableX = tram_addr.NametableX
                                           vram_addr.CoarseX = tram_addr.CoarseX
                                       End If
                                   End Sub

            Dim TransferAddressY = Sub()
                                       If PPUMask.RenderBackground OrElse PPUMask.RenderSprites Then
                                           vram_addr.FineY = tram_addr.FineY
                                           vram_addr.NametableY = tram_addr.NametableY
                                           vram_addr.CoarseY = tram_addr.CoarseY
                                       End If
                                   End Sub

            Dim LoadBackgroundShifters = Sub()
                                             bg_shifter_pattern_lo = (bg_shifter_pattern_lo And &HFF00US) Or CUShort(bg_next_tile_lsb)
                                             bg_shifter_pattern_hi = (bg_shifter_pattern_hi And &HFF00US) Or CUShort(bg_next_tile_msb)

                                             If (bg_next_tile_attrib And &H1) <> 0 Then
                                                 bg_shifter_attrib_lo = (bg_shifter_attrib_lo And &HFF00US) Or &HFFUS
                                             Else
                                                 bg_shifter_attrib_lo = (bg_shifter_attrib_lo And &HFF00US)
                                             End If

                                             If (bg_next_tile_attrib And &H2) <> 0 Then
                                                 bg_shifter_attrib_hi = (bg_shifter_attrib_hi And &HFF00US) Or &HFFUS
                                             Else
                                                 bg_shifter_attrib_hi = (bg_shifter_attrib_hi And &HFF00US)
                                             End If
                                         End Sub

            Dim UpdateShifters = Sub()
                                     If PPUMask.RenderBackground Then
                                         bg_shifter_pattern_lo <<= 1
                                         bg_shifter_pattern_hi <<= 1
                                         bg_shifter_attrib_lo <<= 1
                                         bg_shifter_attrib_hi <<= 1
                                     End If

                                     If PPUMask.RenderSprites AndAlso cycle >= 1 AndAlso cycle < 258 Then
                                         For i As Integer = 0 To Math.Min(sprite_count - 1, 7)
                                             If spriteScanline(i).X > 0 Then
                                                 spriteScanline(i).X -= 1
                                             Else
                                                 sprite_shifter_pattern_lo(i) <<= 1
                                                 sprite_shifter_pattern_hi(i) <<= 1
                                             End If
                                         Next
                                     End If
                                 End Sub

            ' ============================================================================
            ' VISIBLE SCANLINES + PRE-RENDER SCANLINE (-1 to 239)
            ' ============================================================================
            If scanline >= -1 AndAlso scanline < 240 Then

                ' Skip cycle 0 on odd frames when rendering is enabled
                If scanline = 0 AndAlso cycle = 0 AndAlso odd_frame AndAlso (PPUMask.RenderBackground OrElse PPUMask.RenderSprites) Then
                    cycle = 1
                End If

                ' Start of new frame - clear flags
                If scanline = -1 AndAlso cycle = 1 Then
                    FrameNumber += 1
                    PPUStatus.VerticalBlank = False
                    PPUStatus.SpriteOverflow = False
                    PPUStatus.SpriteZeroHit = False

                    For i As Integer = 0 To 7
                        sprite_shifter_pattern_lo(i) = 0
                        sprite_shifter_pattern_hi(i) = 0
                    Next
                End If

                ' Background tile fetching cycles
                If (cycle >= 2 AndAlso cycle < 258) OrElse (cycle >= 321 AndAlso cycle < 338) Then
                    UpdateShifters()

                    Select Case ((cycle - 1) Mod 8)
                        Case 0
                            LoadBackgroundShifters()
                            bg_next_tile_id = ppuRead(&H2000US Or (vram_addr.Reg And &HFFFUS))

                        Case 2
                            ' Fetch attribute byte
                            bg_next_tile_attrib = ppuRead(&H23C0US Or
                                                           (If(vram_addr.NametableY, 1, 0) << 11) Or
                                                           (If(vram_addr.NametableX, 1, 0) << 10) Or
                                                           ((vram_addr.CoarseY >> 2) << 3) Or
                                                           (vram_addr.CoarseX >> 2))

                            ' Extract correct 2 bits based on position within 4x4 tile block
                            Dim shift_amount As Integer = 0
                            If (vram_addr.CoarseY And &H2US) <> 0 Then shift_amount += 4
                            If (vram_addr.CoarseX And &H2US) <> 0 Then shift_amount += 2
                            bg_next_tile_attrib = (bg_next_tile_attrib >> shift_amount) And &H3UI

                        Case 4
                            ' Fetch tile LSB
                            bg_next_tile_lsb = ppuRead(MathHelpers.SafeAddition16(
                                MathHelpers.SafeAddition16(
                                    MathHelpers.SafeShiftLeft16(If(PPUControl.PatternBackground, 1, 0), 12),
                                    MathHelpers.SafeShiftLeft16(bg_next_tile_id, 4)),
                                vram_addr.FineY))

                        Case 6
                            ' Fetch tile MSB
                            bg_next_tile_msb = ppuRead(MathHelpers.SafeAddition16(
                                MathHelpers.SafeAddition16(
                                    MathHelpers.SafeShiftLeft16(If(PPUControl.PatternBackground, 1, 0), 12),
                                    MathHelpers.SafeShiftLeft16(bg_next_tile_id, 4)),
                                MathHelpers.SafeAddition16(vram_addr.FineY, 8)))

                        Case 7
                            IncrementScrollX()
                    End Select
                End If

                ' End of visible scanline - increment Y
                If cycle = 256 Then
                    IncrementScrollY()
                End If

                ' Reset X position
                If cycle = 257 Then
                    LoadBackgroundShifters()
                    TransferAddressX()
                End If

                ' Superfluous nametable fetches
                If cycle = 338 OrElse cycle = 340 Then
                    bg_next_tile_id = ppuRead(&H2000US Or (vram_addr.Reg And &HFFFUS))
                End If

                ' Pre-render scanline - reset Y position
                If scanline = -1 AndAlso cycle >= 280 AndAlso cycle < 305 Then
                    TransferAddressY()
                End If

                ' ========================================================================
                ' SPRITE EVALUATION (cycle 257)
                ' ========================================================================
                If cycle = 257 AndAlso scanline >= 0 Then
                    ' Clear sprite scanline buffer
                    For i As Integer = 0 To 7
                        spriteScanline(i).Fill(&HFFUI)
                    Next

                    sprite_count = 0

                    ' Clear sprite shifters
                    For i As Integer = 0 To 7
                        sprite_shifter_pattern_lo(i) = 0
                        sprite_shifter_pattern_hi(i) = 0
                    Next

                    ' Evaluate sprites for next scanline
                    Dim nOAMEntry As Byte = 0
                    bSpriteZeroHitPossible = False

                    While nOAMEntry < 64 AndAlso sprite_count < 9
                        Dim diff As Integer = scanline - CInt(OAM(nOAMEntry).y)
                        Dim spriteHeight As Integer = If(PPUControl.SpriteSize, 16, 8)

                        If diff >= 0 AndAlso diff < spriteHeight Then
                            If sprite_count < 8 Then
                                If nOAMEntry = 0 Then
                                    bSpriteZeroHitPossible = True
                                End If
                                spriteScanline(sprite_count).CopyFrom(OAM(nOAMEntry))
                            End If
                            sprite_count += 1
                        End If
                        nOAMEntry += 1
                    End While

                    PPUStatus.SpriteOverflow = (sprite_count >= 8)
                End If

                ' ========================================================================
                ' SPRITE PATTERN LOADING (cycle 340)
                ' ========================================================================
                If cycle = 340 Then
                    For i As Integer = 0 To Math.Min(sprite_count - 1, 7)
                        Dim sprite_pattern_bits_lo, sprite_pattern_bits_hi As Byte
                        Dim sprite_pattern_addr_lo, sprite_pattern_addr_hi As UInt16

                        If PPUControl.SpriteSize = 0 Then
                            ' 8x8 sprite mode
                            If (spriteScanline(i).Attributes And &H80UI) = 0 Then
                                ' Not flipped vertically
                                sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(If(PPUControl.PatternSprite, 1, 0), 12) Or
                                                         MathHelpers.SafeShiftLeft16(spriteScanline(i).TileID, 4) Or
                                                         CUShort((scanline - spriteScanline(i).Y) And 7)
                            Else
                                ' Flipped vertically
                                sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(If(PPUControl.PatternSprite, 1, 0), 12) Or
                                                         MathHelpers.SafeShiftLeft16(spriteScanline(i).TileID, 4) Or
                                                         MathHelpers.SafeSubtract16(7, CUShort((scanline - spriteScanline(i).Y) And 7))
                            End If
                        Else
                            ' 8x16 sprite mode
                            If (spriteScanline(i).Attributes And &H80UI) = 0 Then
                                ' Not flipped vertically
                                If (scanline - spriteScanline(i).Y) < 8 Then
                                    ' Top half
                                    sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(CUShort(spriteScanline(i).TileID And &H1UI), 12) Or
                                                             MathHelpers.SafeShiftLeft16(CUShort(spriteScanline(i).TileID And &HFEUI), 4) Or
                                                             CUShort((scanline - spriteScanline(i).Y) And &H7UI)
                                Else
                                    ' Bottom half
                                    sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(CUShort(spriteScanline(i).TileID And &H1UI), 12) Or
                                                             MathHelpers.SafeShiftLeft16(MathHelpers.SafeAddition16(CUShort(spriteScanline(i).TileID And &HFEUI), 1), 4) Or
                                                             CUShort((scanline - spriteScanline(i).Y) And &H7UI)
                                End If
                            Else
                                ' Flipped vertically
                                If (scanline - spriteScanline(i).y) < 8 Then
                                    ' Top half (which is bottom when flipped)
                                    sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(CUShort(spriteScanline(i).TileID And &H1UI), 12) Or
                                                             MathHelpers.SafeShiftLeft16(MathHelpers.SafeAddition16(CUShort(spriteScanline(i).TileID And &HFEUI), 1), 4) Or
                                                             MathHelpers.SafeSubtract16(7, CUShort((scanline - spriteScanline(i).Y) And &H7UI))
                                Else
                                    ' Bottom half (which is top when flipped)
                                    sprite_pattern_addr_lo = MathHelpers.SafeShiftLeft16(CUShort(spriteScanline(i).TileID And &H1UI), 12) Or
                                                             MathHelpers.SafeShiftLeft16(CUShort(spriteScanline(i).TileID And &HFEUI), 4) Or
                                                             MathHelpers.SafeSubtract16(7, CUShort((scanline - spriteScanline(i).Y) And &H7UI))
                                End If
                            End If
                        End If

                        sprite_pattern_addr_hi = MathHelpers.SafeAddition16(sprite_pattern_addr_lo, 8)

                        sprite_pattern_bits_lo = ppuRead(sprite_pattern_addr_lo)
                        sprite_pattern_bits_hi = ppuRead(sprite_pattern_addr_hi)

                        ' Flip horizontally if needed
                        If (spriteScanline(i).Attributes And &H40UI) <> 0 Then
                            Dim flipbyte = Function(b As Byte) As Byte
                                               If b = 0 Then Return 0
                                               b = ((b And &HF0UI) >> 4) Or ((b And &HFUI) << 4)
                                               b = ((b And &HCCUI) >> 2) Or ((b And &H33UI) << 2)
                                               b = ((b And &HAAUI) >> 1) Or ((b And &H55UI) << 1)
                                               Return b
                                           End Function
                            sprite_pattern_bits_lo = flipbyte(sprite_pattern_bits_lo)
                            sprite_pattern_bits_hi = flipbyte(sprite_pattern_bits_hi)
                        End If

                        sprite_shifter_pattern_lo(i) = sprite_pattern_bits_lo
                        sprite_shifter_pattern_hi(i) = sprite_pattern_bits_hi
                    Next
                End If

            End If ' End visible/pre-render scanlines

            ' ============================================================================
            ' POST-RENDER SCANLINE (240) - Do nothing
            ' ============================================================================
            If scanline = 240 Then
                ' Idle scanline
            End If

            ' ============================================================================
            ' VERTICAL BLANK SCANLINES (241-260)
            ' ============================================================================
            If scanline >= 241 AndAlso scanline < 261 Then
                If scanline = 241 AndAlso cycle = 1 Then
                    PPUStatus.VerticalBlank = True
                    If PPUControl.EnableNmi Then
                        nmi = True
                    End If
                End If
            End If

            ' ============================================================================
            ' PIXEL RENDERING - Happens every cycle during visible scanlines
            ' ============================================================================
            Dim bg_pixel As Byte = 0
            Dim bg_palette As Byte = 0
            Dim fg_pixel As Byte = 0
            Dim fg_palette As Byte = 0
            Dim fg_priority As Byte = 0

            ' Render background pixel
            If PPUMask.RenderBackground <> 0 Then
                If PPUMask.RenderBackgroundLeft <> 0 OrElse (cycle >= 9) Then
                    Dim bit_mux As UInt16 = &H8000US >> fine_x

                    Dim p0_pixel As Byte = If((bg_shifter_pattern_lo And bit_mux) <> 0, 1, 0)
                    Dim p1_pixel As Byte = If((bg_shifter_pattern_hi And bit_mux) <> 0, 1, 0)
                    bg_pixel = (p1_pixel << 1) Or p0_pixel

                    Dim bg_pal0 As Byte = If((bg_shifter_attrib_lo And bit_mux) <> 0, 1, 0)
                    Dim bg_pal1 As Byte = If((bg_shifter_attrib_hi And bit_mux) <> 0, 1, 0)
                    bg_palette = (bg_pal1 << 1) Or bg_pal0
                End If
            End If

            ' Render foreground (sprite) pixel
            If PPUMask.RenderSprites <> 0 Then
                If PPUMask.RenderSpritesLeft <> 0 OrElse (cycle >= 9) Then
                    bSpriteZeroBeingRendered = False

                    For i As Integer = 0 To Math.Min(sprite_count - 1, 7)
                        If spriteScanline(i).X = 0 Then
                            Dim fg_pixel_lo As Byte = If((sprite_shifter_pattern_lo(i) And &H80UI) <> 0, 1, 0)
                            Dim fg_pixel_hi As Byte = If((sprite_shifter_pattern_hi(i) And &H80UI) <> 0, 1, 0)
                            fg_pixel = (fg_pixel_hi << 1) Or fg_pixel_lo

                            fg_palette = (spriteScanline(i).Attributes And &H3UI) + &H4UI
                            fg_priority = If((spriteScanline(i).Attributes And &H20UI) = 0, 1, 0)

                            If fg_pixel <> 0 Then
                                If i = 0 Then
                                    bSpriteZeroBeingRendered = True
                                End If
                                Exit For
                            End If
                        End If
                    Next
                End If
            End If

            ' Combine background and foreground
            Dim pixel As Byte = 0
            Dim palette As Byte = 0

            If bg_pixel = 0 AndAlso fg_pixel = 0 Then
                pixel = 0
                palette = 0
            ElseIf bg_pixel = 0 AndAlso fg_pixel > 0 Then
                pixel = fg_pixel
                palette = fg_palette
            ElseIf bg_pixel > 0 AndAlso fg_pixel = 0 Then
                pixel = bg_pixel
                palette = bg_palette
            ElseIf bg_pixel > 0 AndAlso fg_pixel > 0 Then
                If fg_priority <> 0 Then
                    pixel = fg_pixel
                    palette = fg_palette
                Else
                    pixel = bg_pixel
                    palette = bg_palette
                End If

                ' Sprite zero hit detection
                If bSpriteZeroHitPossible AndAlso bSpriteZeroBeingRendered Then
                    If (PPUMask.RenderBackground <> 0) AndAlso (PPUMask.RenderSprites <> 0) Then
                        If (PPUMask.RenderBackgroundLeft Or PPUMask.RenderSpritesLeft) = 0 Then
                            If cycle >= 9 AndAlso cycle < 258 Then
                                PPUStatus.SpriteZeroHit = True
                            End If
                        Else
                            If cycle >= 1 AndAlso cycle < 258 Then
                                PPUStatus.SpriteZeroHit = True
                            End If
                        End If
                    End If
                End If
            End If

            ' Draw the pixel (only during visible area)
            If scanline >= 0 AndAlso scanline < 240 AndAlso cycle >= 1 AndAlso cycle < 257 Then
                sprScreen.SetPixel(cycle - 1, scanline, GetColorFromPaletteRam(palette, pixel))
            End If

            ' Advance cycle and scanline
            cycle += 1

            If PPUMask.RenderBackground <> 0 OrElse PPUMask.RenderSprites <> 0 Then
                If cycle = 260 AndAlso scanline < 240 Then
                    Cart.GetMapper.ScanlineCounter()
                End If
            End If

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