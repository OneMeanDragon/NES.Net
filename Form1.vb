Imports System.IO
Imports System.Threading
Imports System.Drawing.Imaging
Imports System.Runtime.InteropServices

Imports Nintendo.FOREVERLOOP_HELPERS
Imports Nintendo.GraphicsObjects
Imports Nintendo.NintendoEntertainmentSystem

Public Class Form1
#Region "Project Registry Information"
    'Since I already have this registry key im just going to use it.
    Public Const strProgramTitle As String = "NES Emulator"
    Public Const strRegistryPath As String = "Software\Visual Basic NES Emulator"
    Public WriteConfig As Microsoft.Win32.RegistryKey
    Public ReadConfig As Microsoft.Win32.RegistryKey
#End Region

#Region "File Information"
    Public strFilename As String
    Public strFilepath As String
#End Region

#Region "Emulation Information"
    Private emNES As New NintendoEntertainmentSystem.clsBus
    'Private Shared emCart As New NintendoEntertainmentSystem.clsCartridge()
#End Region

    Public Shared running As Boolean = False
    Private Shared bmpBackground As Bitmap 'Figure out a way to hold that ratio


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Initalize Everything that we need
        ' Cart = New NintendoEntertainmentSystem.clsCartridge()
    End Sub
    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        'Stop the emulation if its running
        '^
    End Sub

    Private Sub Form1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles Me.KeyPress
        If e.KeyChar = "p" Then
            If nSelectedPalette + 1 > 7 Then
                nSelectedPalette = 0
            Else
                nSelectedPalette += 1
            End If
        End If
    End Sub

    Private Sub Form1_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        'Direct these at the active emulator
        'nes.controller[0] |= GetKey(olc:Key : X).bHeld ? 0x80 : 0x00;     // A Button
        'nes.controller[0] |= GetKey(olc:Key : Z).bHeld ? 0x40 : 0x00;     // B Button
        'nes.controller[0] |= GetKey(olc:Key : A).bHeld ? 0x20 : 0x00;     // Select
        'nes.controller[0] |= GetKey(olc:Key : S).bHeld ? 0x10 : 0x00;     // Start
        'nes.controller[0] |= GetKey(olc:Key : UP).bHeld ? 0x08 : 0x00;
        'nes.controller[0] |= GetKey(olc:Key : DOWN).bHeld ? 0x04 : 0x00;
        'nes.controller[0] |= GetKey(olc:Key : Left).bHeld ? 0x02 : 0x00;
        'nes.controller[0] |= GetKey(olc:Key : Right).bHeld ? 0x01 : 0x00;

        Select Case e.KeyCode
            Case Keys.W
                emNES.Controller(0) = emNES.Controller(0) Or &H8
            Case Keys.S
                emNES.Controller(0) = emNES.Controller(0) Or &H4
            Case Keys.A
                emNES.Controller(0) = emNES.Controller(0) Or &H2
            Case Keys.D
                emNES.Controller(0) = emNES.Controller(0) Or &H1
            Case Keys.J
                emNES.Controller(0) = emNES.Controller(0) Or &H20
            Case Keys.K
                emNES.Controller(0) = emNES.Controller(0) Or &H10
            Case Keys.N
                emNES.Controller(0) = emNES.Controller(0) Or &H80
            Case Keys.M
                emNES.Controller(0) = emNES.Controller(0) Or &H40
        End Select

    End Sub

    Private Sub Form1_KeyUp(sender As Object, e As KeyEventArgs) Handles Me.KeyUp
        'Direct these at the active emulator
        Select Case e.KeyCode
            Case Keys.W
                emNES.Controller(0) = emNES.Controller(0) Xor &H8
            Case Keys.S
                emNES.Controller(0) = emNES.Controller(0) Xor &H4
            Case Keys.A
                emNES.Controller(0) = emNES.Controller(0) Xor &H2
            Case Keys.D
                emNES.Controller(0) = emNES.Controller(0) Xor &H1
            Case Keys.J
                emNES.Controller(0) = emNES.Controller(0) Xor &H20
            Case Keys.K
                emNES.Controller(0) = emNES.Controller(0) Xor &H10
            Case Keys.N
                emNES.Controller(0) = emNES.Controller(0) Xor &H80
            Case Keys.M
                emNES.Controller(0) = emNES.Controller(0) Xor &H40
        End Select

    End Sub

    Private Sub OpenToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenToolStripMenuItem.Click
        'check if currently emulating..

        'Close old nes file

        '
        WriteConfig = Microsoft.Win32.Registry.CurrentUser.CreateSubKey(strRegistryPath)
        ReadConfig = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(strRegistryPath)

        dlgOpenFile.InitialDirectory = ReadConfig.GetValue("LastDirectory")
        dlgOpenFile.Filter = "NES Files (*.nes)|*.nes|All files (*.*)|*.*"
        dlgOpenFile.FilterIndex = 0

        If dlgOpenFile.ShowDialog() = Windows.Forms.DialogResult.OK Then
            strFilename = Path.GetFileName(dlgOpenFile.FileName)
            strFilepath = Path.GetDirectoryName(dlgOpenFile.FileName)
            WriteConfig.SetValue("LastDirectory", strFilepath)
            Me.Text = strProgramTitle & " - " & strFilename
            'LoadNESROM(dlgOpenFile.FileName)
            'SYS.Reset()
            'PPU.PPUFrames = 0 : intFrames = 0
            'tmrSpeed.Enabled = True
            'StartEmulation()
            'StartVideo()
        End If

        'Need to Check if were currently emulating
        If running Then Return
        If IsNothing(Cart) Then
            Cart = New clsCartridge(dlgOpenFile.FileName)
            If Cart.ValidImage Then
                emNES.Reset()
                emNES.PPU.DiagnoseAttributeTable()
                ' quick test: enable background+sprites (mask bit 3 = render_background, bit 4 = render_sprites -> 0x18)
                '    emNES.PPU.Debug_SetPPUMask(&H18)
                ' Fill palette and nametables so the PPU has visible data immediately
                '    emNES.PPU.Debug_FillPaletteSequential()
                '    emNES.PPU.Debug_FillNameTables(&H0) '24) ' sample tile id used earlier
            End If
        Else
            Cart.Reset()
            Cart.LoadCartridge(dlgOpenFile.FileName)
            If Cart.ValidImage Then
                emNES.Reset()
            End If
        End If

        If Not Cart.ValidImage Then
            Return
        End If
        'Create Thread for Run()
        If Not IsNothing(VideoThread) Then : VideoThread = Nothing : End If
        VideoThread = New System.Threading.Thread(AddressOf Run)
        VideoThread.IsBackground = True
        VideoThread.Start()

    End Sub
    Public Shared nSelectedPalette As UInteger = 0

    Private VideoThread As Thread
    Public Sub Run()
        'Const FRAMERATE_LOCK As UInteger = (1000 / 100)
        Dim CapTimer As New myTimer()
        Dim frame_start, frame_end As Integer

        Dim n_PrevSelectedPallet As Integer = nSelectedPalette
        running = True


        DrawClear(PixelColors.DARK_GREY) 'if left in the main loop screen flashes
        'Draw the Base Screen

        'Queue the patterns to be drawn
        Dim QueuePatterns As Boolean = True
        Dim QueuePalettes As Boolean = True

        While running
            CapTimer.StartMe()
            frame_start = Environment.TickCount

            Dim cpuCycles As Integer = 0
            Dim vblankCycles As Integer = 0
            Dim inVBlank As Boolean = False

            emNES.PPU.frame_complete = False
            Do
                emNES.Clock()
                ClockCounter += 1

                If (ClockCounter Mod 3) = 0 Then
                    cpuCycles += 1
                    If emNES.PPU.Debug_Scanline >= 241 AndAlso emNES.PPU.Debug_Scanline < 261 Then
                        vblankCycles += 1
                    End If
                End If

                If running = False Then
                    emNES.Reset()
                    Exit While
                    'Exit Do
                End If
            Loop While Not emNES.PPU.frame_complete
            Debug.WriteLine(String.Format("Frame: Total CPU cycles={0}, VBlank CPU cycles={1}",
                                  cpuCycles, vblankCycles))

            ' Completed Frame
            'DebugDumpPPUState()
            'DumpPPUFullDebug()

            ' Reset the frame
            emNES.PPU.frame_complete = False

            frame_end = Environment.TickCount
            Debug.WriteLine("NES Frame completed in: " & ((frame_end - frame_start) / 1000).ToString() & " Seconds.")
            'Attempt to draw to the screen picture box
            '// Draw rendered output ========================================================

            ' Draw the Patterns
            If QueuePatterns Then
                DrawSprite(256 + 4, 2, emNES.PPU.GetPatternTable(0, nSelectedPalette))                  ' Working (would like to resize these to 1/2 scale to make room on the screen)
                DrawSprite(256 + 4 + (128 + 2), 2, emNES.PPU.GetPatternTable(1, nSelectedPalette))      ' Working
                ' Drawing half scaled
                'DrawToScale(256 + 4, 2, emNES.PPU.GetPatternTable(0, nSelectedPalette), 0.5)        'Also working, bitmap scaleing sucks though lol 0.5 = (128x128)=64x64
                'DrawToScale(256 + 6 + 64, 2, emNES.PPU.GetPatternTable(1, nSelectedPalette), 0.5)   'Also working, bitmap scaleing sucks though lol 0.5 = (128x128)=64x64
                QueuePatterns = False
            End If

            Const nSwatchSize As Integer = 6
            ' Draw the selection reticule around the selected [to hell with line drawing]
            'If we changed selection fill in the old rect
            If n_PrevSelectedPallet <> nSelectedPalette Then
                FillRect((256 + 4) + 1 + (n_PrevSelectedPallet * (nSwatchSize * 5)), 132, (nSwatchSize * 4), nSwatchSize + 2, PixelColors.DARK_GREY) 'Set to previous color
                n_PrevSelectedPallet = nSelectedPalette
                QueuePatterns = True 'update the paterns with the new palette
                QueuePalettes = True
            End If
            If QueuePalettes Then
                FillRect((256 + 4) + 1 + (nSelectedPalette * (nSwatchSize * 5)), 132, (nSwatchSize * 4), nSwatchSize + 2, PixelColors.CYAN)
                ' Draw the Palettes under all that
                For p As Integer = 0 To 7
                    For s As Integer = 0 To 3
                        FillRect((256 + 4) + 1 + p * (nSwatchSize * 5) + s * nSwatchSize, 133, nSwatchSize, nSwatchSize, emNES.PPU.GetColorFromPaletteRam(p, s))
                    Next
                Next
                QueuePalettes = False
            End If

            If Not debugDumped Then
                'emNES.PPU.Debug_FillPaletteSequential()
                'emNES.PPU.Debug_FillNameTables(&H0)  ' use tile 0x00 which you confirmed contains CHR
                'emNES.PPU.Debug_SetPPUMask(&H18)      ' enable background + sprites
                DebugDumpPPUState()
                debugDumped = True
            End If
            ' Temporary debug: force visible PPU output (remove after verification)
            'If Not debugDumped Then
            '    emNES.PPU.Debug_FillPaletteSequential()
            '    emNES.PPU.Debug_FillNameTables(&H0)  ' use tile 0x00 which you confirmed contains CHR
            '    emNES.PPU.Debug_SetPPUMask(&H18)      ' enable background + sprites
            '    debugDumped = True
            'End If

            ' Render the screen
            DrawSprite(2, 2, emNES.PPU.GetScreen()) 'not working


            '----------------------
            'For i As Integer = 0 To 4
            '   For j As Integer = 0 To 4
            '       Dim id As Byte = emNES.PPU.tblName(0, i * 32 + j)
            '       DrawSprite(i * 16, j * 16, emNES.PPU.GetPatternTable(0, nSelectedPalette))
            '   Next
            'Next



            Debug.WriteLine("FPS:" & CapTimer.CalculateFPS().ToString())
            'Sleep Frames or not (Not needed ever since adding the draw procedure, was running 900FPS without it, now its lucky to make 3FPS at times lol, though my calculation in the timer is likely wrong aswell)
            'Dim frameticks As UInteger = CapTimer.GetDelta()
            'If frameticks < FRAMERATE_LOCK Then 
            '    Threading.Thread.Sleep(FRAMERATE_LOCK - frameticks)
            'End If
        End While
        DrawClear(PixelColors.BLACK)
    End Sub

    '256=Game screen size
    '4=Extra width /2 on either side of the game screen itself
    '128+2=Full scale size of the Pattern Box + marginright and there is 2 of them
    Const BMP_WIDTH As Integer = 256 + 4 + ((128 + 2) * 2)

    '240=Game Screen height
    '4=extra space /2 above and below the screen
    Const BMP_HEIGHT As Integer = 240 + 4 '+ 800

    'Private Sub DrawPixel(ByVal x As UInt32, ByVal y As UInt32, ByVal p As Pixel)
    '    If IsNothing(bmpBackground) Then
    '        bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT)
    '    End If
    '    bmpBackground.SetPixel(x, y, Color.FromArgb(p.m_Pixel.Signed))
    'End Sub

    Private Sub DrawPixel(ByVal x As UInt32, ByVal y As UInt32, ByVal p As Pixel)
        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT)
        End If

        ' Convert indices to Int32 to satisfy SetPixel and avoid overflow conversions.
        Dim xi As Integer = CInt(x)
        Dim yi As Integer = CInt(y)

        ' Use explicit ARGB components to avoid relying on packed Signed integer interpretation.
        Dim col As Color = Color.FromArgb(p.m_Pixel.a, p.m_Pixel.r, p.m_Pixel.g, p.m_Pixel.b)

        bmpBackground.SetPixel(xi, yi, col)
    End Sub

    Private Sub FillRect(ByVal x As UInt32, ByVal y As UInt32, ByVal w As UInt32, ByVal h As UInt32, ByVal p As Pixel)
        Dim x2 As UInt32 = x + w
        Dim y2 As UInt32 = y + h

        If x <= 0 Then x = 0
        If x >= BMP_WIDTH Then x = BMP_WIDTH
        If y <= 0 Then y = 0
        If y >= BMP_HEIGHT Then y = BMP_HEIGHT

        If x2 <= 0 Then x2 = 0
        If x2 >= BMP_WIDTH Then x2 = BMP_WIDTH
        If y2 <= 0 Then y2 = 0
        If y2 >= BMP_HEIGHT Then y2 = BMP_HEIGHT

        For i As UInt32 = x To x2 - 1
            For j As UInt32 = y To y2 - 1
                DrawPixel(i, j, p)
            Next
        Next

    End Sub

    Private Sub DrawToScale(ByVal x As UInt32, ByVal y As UInt32, ByVal objSprite As Sprite, Optional ByVal scale As Single = 1.0F)
        Dim tempBMP As Bitmap
        If objSprite.Height > 0 AndAlso objSprite.Width > 0 Then
            If scale = 1.0F Then 'send to the normal draw were drawing at full scale
                DrawSprite(CInt(x), CInt(y), objSprite)
                Return
            End If
            Dim h As Integer = objSprite.Height
            Dim w As Integer = objSprite.Width
            tempBMP = New Bitmap(w, h)
        Else
            Return
        End If
        'Draw to the Temp BMP
        For i As Integer = 0 To tempBMP.Width - 1
            ' FIX: iterate height for the inner loop (was Width in original)
            For j As Integer = 0 To tempBMP.Height - 1
                tempBMP.SetPixel(i, j, Color.FromArgb(objSprite.GetPixel(i, j).m_Pixel.a, objSprite.GetPixel(i, j).m_Pixel.r, objSprite.GetPixel(i, j).m_Pixel.g, objSprite.GetPixel(i, j).m_Pixel.b))
            Next
        Next
        'Resize the newly made bmp
        Dim resizedBMP As New Bitmap(tempBMP, CInt(objSprite.Width * scale), CInt(objSprite.Height * scale))
        ' Draw the new Image to the screen buffer
        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT)
        End If

        For i As Int32 = 0 To resizedBMP.Width - 1
            For j As Int32 = 0 To resizedBMP.Height - 1
                Dim dstX As Integer = CInt(x) + i
                Dim dstY As Integer = CInt(y) + j
                If dstX >= 0 AndAlso dstX < bmpBackground.Width AndAlso dstY >= 0 AndAlso dstY < bmpBackground.Height Then
                    bmpBackground.SetPixel(dstX, dstY, resizedBMP.GetPixel(i, j))
                End If
            Next
        Next

        'Dispose our temporary bitmaps
        tempBMP.Dispose()
        resizedBMP.Dispose()

        Dim sendBmp As Bitmap = CType(bmpBackground.Clone(), Bitmap)
        If picScreen.InvokeRequired Then
            picScreen.Invoke(New DoStuffDelegate(AddressOf picScreenDel), sendBmp)
        Else
            picScreenDel(sendBmp)
        End If
    End Sub

    Private Sub DrawClear(ByVal p As Pixel)
        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT)
        End If

        Dim col As Color = Color.FromArgb(p.m_Pixel.a, p.m_Pixel.r, p.m_Pixel.g, p.m_Pixel.b)

        For i As Int32 = 0 To bmpBackground.Width - 1
            For j As Int32 = 0 To bmpBackground.Height - 1
                bmpBackground.SetPixel(i, j, col)
            Next
        Next

        Dim sendBmp As Bitmap = CType(bmpBackground.Clone(), Bitmap)
        If picScreen.InvokeRequired Then
            picScreen.Invoke(New DoStuffDelegate(AddressOf picScreenDel), sendBmp)
        Else
            picScreenDel(sendBmp)
        End If
    End Sub

    Private Sub DrawSprite(ByVal x As Int32, ByVal y As Int32, ByVal ImageObj As GraphicsObjects.Sprite)
        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT, PixelFormat.Format32bppArgb)
        End If

        Dim w As Integer = CInt(ImageObj.Width)
        Dim h As Integer = CInt(ImageObj.Height)

        ' Clip source region to destination bitmap
        If x + w <= 0 OrElse y + h <= 0 OrElse x >= bmpBackground.Width OrElse y >= bmpBackground.Height Then
            ' nothing visible
            Return
        End If

        ' compute effective copy rectangle
        Dim srcX As Integer = 0
        Dim srcY As Integer = 0
        Dim dstX As Integer = x
        Dim dstY As Integer = y
        Dim copyW As Integer = w
        Dim copyH As Integer = h

        If dstX < 0 Then
            srcX = -dstX
            copyW -= srcX
            dstX = 0
        End If
        If dstY < 0 Then
            srcY = -dstY
            copyH -= srcY
            dstY = 0
        End If
        If dstX + copyW > bmpBackground.Width Then
            copyW = bmpBackground.Width - dstX
        End If
        If dstY + copyH > bmpBackground.Height Then
            copyH = bmpBackground.Height - dstY
        End If
        If copyW <= 0 OrElse copyH <= 0 Then Return

        Dim bmpRect As New Rectangle(0, 0, bmpBackground.Width, bmpBackground.Height)
        Dim bmpData As BitmapData = bmpBackground.LockBits(bmpRect, ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)
        Try
            Dim stride As Integer = bmpData.Stride
            Dim basePtr As IntPtr = bmpData.Scan0

            ' Row buffer reused to reduce allocations
            Dim rowBytes(copyW * 4 - 1) As Byte

            For row As Integer = 0 To copyH - 1
                Dim srcRow As Integer = srcY + row
                Dim dstRow As Integer = dstY + row

                ' Fill rowBytes from sprite pixels
                Dim jOff As Integer = 0
                For col As Integer = 0 To copyW - 1
                    Dim srcCol As Integer = srcX + col
                    Dim px As GraphicsObjects.Pixel = ImageObj.GetPixel(srcCol, srcRow)
                    rowBytes(jOff) = px.m_Pixel.b
                    rowBytes(jOff + 1) = px.m_Pixel.g
                    rowBytes(jOff + 2) = px.m_Pixel.r
                    rowBytes(jOff + 3) = px.m_Pixel.a
                    jOff += 4
                Next

                ' destination pointer for this row
                Dim destOffset As Integer = (dstRow * stride) + (dstX * 4)
                Dim destPtr As IntPtr = New IntPtr(basePtr.ToInt64() + destOffset)
                Marshal.Copy(rowBytes, 0, destPtr, rowBytes.Length)
            Next
        Finally
            bmpBackground.UnlockBits(bmpData)
        End Try

        ' send a cloned bitmap to UI thread to avoid concurrent access
        Dim sendBmp As Bitmap = CType(bmpBackground.Clone(), Bitmap)
        If picScreen.InvokeRequired Then
            picScreen.Invoke(New DoStuffDelegate(AddressOf picScreenDel), sendBmp)
        Else
            picScreenDel(sendBmp)
        End If
    End Sub

    ' Debug helper — save a sprite to disk so you can inspect the PPU output directly.
    Private Sub SaveSpriteToFile(spr As GraphicsObjects.Sprite, filepath As String)
        Dim w As Integer = CInt(spr.Width)
        Dim h As Integer = CInt(spr.Height)
        Using bmp As New Bitmap(w, h, PixelFormat.Format32bppArgb)
            Dim rect As New Rectangle(0, 0, w, h)
            Dim data As BitmapData = bmp.LockBits(rect, ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)
            Try
                Dim stride As Integer = data.Stride
                Dim basePtr As IntPtr = data.Scan0
                Dim rowBytes(w * 4 - 1) As Byte
                For y As Integer = 0 To h - 1
                    Dim off As Integer = 0
                    For x As Integer = 0 To w - 1
                        Dim px = spr.GetPixel(x, y)
                        rowBytes(off) = px.m_Pixel.b
                        rowBytes(off + 1) = px.m_Pixel.g
                        rowBytes(off + 2) = px.m_Pixel.r
                        rowBytes(off + 3) = px.m_Pixel.a
                        off += 4
                    Next
                    Dim destPtr As IntPtr = New IntPtr(basePtr.ToInt64() + (y * stride))
                    Marshal.Copy(rowBytes, 0, destPtr, rowBytes.Length)
                Next
            Finally
                bmp.UnlockBits(data)
            End Try
            bmp.Save(filepath, Imaging.ImageFormat.Png)
        End Using
    End Sub

    ' Diagnostic helper: dump full PPU tables to the Debug output for offline inspection.
    Private Sub DumpPPUFullDebug()
        If IsNothing(emNES) OrElse IsNothing(emNES.PPU) Then
            Debug.WriteLine("DumpPPUFullDebug: emNES.PPU is Nothing")
            Return
        End If

        Try
            Debug.WriteLine("=== PPU FULL DUMP START ===")
            Debug.WriteLine(String.Format("PPUControl={0:X2} PPUMask={1:X2} PPUStatus={2:X2}", emNES.PPU.Debug_PPUControlReg, emNES.PPU.Debug_PPUMaskReg, emNES.PPU.Debug_PPUStatusReg))
            Debug.WriteLine(String.Format("vram_addr=0x{0:X4} tram_addr=0x{1:X4} fine_x={2}", emNES.PPU.Debug_VramReg, emNES.PPU.Debug_TramReg, GetType(NintendoEntertainmentSystem.em2C02).GetField("fine_x", Reflection.BindingFlags.NonPublic Or Reflection.BindingFlags.Instance)?.GetValue(emNES.PPU)))

            ' Palette dump (32 bytes)
            Dim pal() As Byte = emNES.PPU.Debug_GetTblPalette()
            Dim sbPal As New System.Text.StringBuilder()
            sbPal.Append("tblPalette[0..31]:")
            For i As Integer = 0 To pal.Length - 1
                sbPal.Append(" " & pal(i).ToString("X2"))
            Next
            Debug.WriteLine(sbPal.ToString())

            ' Dump entire nametable 0 & 1 (1024 bytes each)
            Dim nt0() As Byte = emNES.PPU.Debug_GetNameTableRow(0, 0, 1024)
            Debug.WriteLine("NameTable0 (1024 bytes):")
            For i As Integer = 0 To 1023 Step 32
                Dim line As New System.Text.StringBuilder()
                For j As Integer = 0 To 31
                    line.Append(nt0(i + j).ToString("X2") & " ")
                Next
                Debug.WriteLine(line.ToString())
            Next

            Dim nt1() As Byte = emNES.PPU.Debug_GetNameTableRow(1, 0, 1024)
            Debug.WriteLine("NameTable1 (1024 bytes):")
            For i As Integer = 0 To 1023 Step 32
                Dim line As New System.Text.StringBuilder()
                For j As Integer = 0 To 31
                    line.Append(nt1(i + j).ToString("X2") & " ")
                Next
                Debug.WriteLine(line.ToString())
            Next

            ' Dump attribute table bytes for name table 0 ($23C0..$23FF)
            Debug.WriteLine("Attribute bytes for NT0 ($23C0..$23FF):")
            For i As Integer = 0 To 63 Step 8
                Dim line As New System.Text.StringBuilder()
                For j As Integer = 0 To 7
                    Dim addr As UShort = CUShort(&H23C0 + i + j)
                    Dim b As Byte = emNES.PPU.ppuRead(addr)
                    line.Append(b.ToString("X2") & " ")
                Next
                Debug.WriteLine(line.ToString())
            Next

            ' Dump pattern bytes around tile ids likely used by title screen
            ' Adjust startTile/count if you want to inspect specific ids
            Dim startTile As Integer = 0 ' change if you know offending tile id
            Dim tileCount As Integer = 128
            Debug.WriteLine(String.Format("Pattern bytes for tiles {0}..{1} (first 16 bytes each):", startTile, startTile + tileCount - 1))
            For t As Integer = startTile To Math.Min(startTile + tileCount - 1, 255)
                Dim addrBase As UShort = CUShort(((t And &HFF) << 4)) ' assume pattern table 0 here
                Dim sbp As New System.Text.StringBuilder()
                sbp.AppendFormat("Tile {0:X2}:", t)
                For k As Integer = 0 To 15
                    Dim b As Byte = emNES.PPU.ppuRead(CUShort(&H0US + addrBase + k))
                    sbp.Append(" " & b.ToString("X2"))
                Next
                Debug.WriteLine(sbp.ToString())
            Next

            Debug.WriteLine("=== PPU FULL DUMP END ===")
        Catch ex As Exception
            Debug.WriteLine("DumpPPUFullDebug failed: " & ex.Message)
        End Try
    End Sub

    Private Sub DiagnoseNMI()
        ' Read NMI vector from $FFFA/$FFFB
        Dim nmiLow As Byte = emNES.cpuRead(&HFFFAUS)
        Dim nmiHigh As Byte = emNES.cpuRead(&HFFFBUS)
        Dim nmiVector As UInt16 = CUShort((nmiHigh << 8) Or nmiLow)

        Debug.WriteLine(String.Format("NMI Vector: $FFFA=${0:X2}, $FFFB=${1:X2} → NMI handler at ${2:X4}",
                                  nmiLow, nmiHigh, nmiVector))

        ' Read first few bytes of NMI handler
        Debug.WriteLine("First 16 bytes of NMI handler:")
        For i As Integer = 0 To 15
            Dim b As Byte = emNES.cpuRead(CUShort(nmiVector + i))
            Debug.Write(String.Format("{0:X2} ", b))
            If (i + 1) Mod 8 = 0 Then Debug.WriteLine("")
        Next
        Debug.WriteLine("")
    End Sub

    Private Delegate Sub DoStuffDelegate(bg As Bitmap)
    Sub picScreenDel(bg As Bitmap)
        picScreen.Image = bg
        picScreen.Refresh()
    End Sub

    Private Sub StopToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles StopToolStripMenuItem.Click
        running = False
    End Sub

    ' Diagnostic helpers - add inside the Form1 class

    Private debugDumped As Boolean = False

    ' Dump CHR range from the cartridge for inspection (safe, non-destructive)
    Private Sub DumpCartCHR(startAddr As Integer, length As Integer)
        If IsNothing(Cart) Then
            Debug.WriteLine("DumpCartCHR: Cart is Nothing")
            Return
        End If
        Dim sb As New System.Text.StringBuilder()
        sb.AppendFormat("Cart CHR dump 0x{0:X4}..0x{1:X4}:", startAddr, startAddr + length - 1)
        Debug.WriteLine(sb.ToString())
        For i As Integer = 0 To length - 1
            Dim addr As UShort = CUShort((startAddr + i) And &H3FFFUS)
            Dim b As Byte = 0
            Dim ok As Boolean = False
            Try
                ok = Cart.ppuRead(addr, b)
            Catch ex As Exception
                Debug.WriteLine("Cart.ppuRead threw: " & ex.Message)
            End Try
            Debug.WriteLine(String.Format("  0x{0:X4}: {1} 0x{2:X2}", addr, If(ok, "OK", "NO"), b))
        Next
    End Sub

    ' Update DebugDumpPPUState to use em2C02 debug accessors
    Private Sub DebugDumpPPUState()
        Try
            Dim spr As GraphicsObjects.Sprite = emNES.PPU.GetScreen()
            Dim outPath As String = Path.Combine("./", "ppu_frame.png") 'Path.GetTempPath()
            SaveSpriteToFile(spr, outPath)
            Debug.WriteLine("Saved PPU frame to: " & outPath)

            ' PPU accessors
            Debug.WriteLine(String.Format("PPUControl.reg = 0x{0:X2}", emNES.PPU.Debug_PPUControlReg))
            Debug.WriteLine(String.Format("PPUMask.reg    = 0x{0:X2}", emNES.PPU.Debug_PPUMaskReg))
            Debug.WriteLine(String.Format("PPUStatus.reg  = 0x{0:X2}", emNES.PPU.Debug_PPUStatusReg))
            Debug.WriteLine(String.Format("vram_addr.Reg = 0x{0:X4}", emNES.PPU.Debug_VramReg))
            Debug.WriteLine(String.Format("tram_addr.Reg = 0x{0:X4}", emNES.PPU.Debug_TramReg))

            ' Palette & nametable via accessors
            Dim pal() As Byte = emNES.PPU.Debug_GetTblPalette()
            Dim sb As New System.Text.StringBuilder()
            sb.Append("tblPalette[0..31]:")
            For i As Integer = 0 To Math.Min(31, pal.Length - 1)
                sb.Append(" " & pal(i).ToString("X2"))
            Next
            Debug.WriteLine(sb.ToString())

            Dim nt() As Byte = emNES.PPU.Debug_GetNameTableRow(0, 0, 64)
            Dim sbn As New System.Text.StringBuilder()
            sbn.Append("tblName(0)[0..63]:")
            For i As Integer = 0 To nt.Length - 1
                sbn.Append(" " & nt(i).ToString("X2"))
            Next
            Debug.WriteLine(sbn.ToString())

            ' pattern memory copy (internal) - may be zero for CHR-ROM carts
            Dim pat() As Byte = emNES.PPU.Debug_GetPatternBytes(0, 0, 16)
            Dim sbp As New System.Text.StringBuilder()
            sbp.Append("tblPattern(0)[0..15]:")
            For i As Integer = 0 To pat.Length - 1
                sbp.Append(" " & pat(i).ToString("X2"))
            Next
            Debug.WriteLine(sbp.ToString())

            ' Determine background pattern base and first tile id
            Dim bgPatternBase As Integer = If((emNES.PPU.Debug_PPUControlReg And &H10) <> 0, &H1000, &H0)
            Debug.WriteLine(String.Format("Background pattern base = 0x{0:X4}", bgPatternBase))

            Dim firstTile As Integer = If(nt.Length > 0, nt(0), 0)
            Debug.WriteLine(String.Format("NameTable0 first tile id = 0x{0:X2} ({1})", firstTile, firstTile))

            Dim tileAddr As Integer = bgPatternBase + (firstTile * 16)
            Debug.WriteLine(String.Format("Pattern bytes for tile 0x{0:X2} start at PPU addr 0x{1:X4}", firstTile, tileAddr))

            ' Dump CHR around the tile to see whether cartridge has non-zero data there
            DumpCartCHR(tileAddr And &H1FFF, 64) ' show 64 bytes around the pattern address

            ' Read pattern bytes via PPU.ppuRead (already done) and also call cartridge directly
            Dim patternViaPpu As New System.Text.StringBuilder()
            patternViaPpu.Append("ppuRead pattern[0..15]:")
            For i As Integer = 0 To 15
                Dim b As Byte = emNES.PPU.ppuRead(CUShort((tileAddr + i) And &H3FFFUS))
                patternViaPpu.Append(" " & b.ToString("X2"))
            Next
            Debug.WriteLine(patternViaPpu.ToString())

            ' Now probe Cartridge directly (if present) to see if it answers CHR reads
            Try
                If Not IsNothing(Cart) Then
                    Debug.WriteLine("Cart object present.")
                    Try
                        Debug.WriteLine("Cart.ValidImage = " & Cart.ValidImage.ToString())
                    Catch ex As Exception
                        Debug.WriteLine("Cart.ValidImage not accessible: " & ex.Message)
                    End Try

                    For i As Integer = 0 To 15
                        Dim addr As UShort = CUShort((tileAddr + i) And &H3FFFUS)
                        Dim outb As Byte = 0
                        Dim ok As Boolean = False
                        Try
                            ok = Cart.ppuRead(addr, outb)
                        Catch ex As Exception
                            Debug.WriteLine("Cart.ppuRead threw: " & ex.Message)
                        End Try
                        Debug.WriteLine(String.Format("Cart.ppuRead(0x{0:X4}) returned {1}, value 0x{2:X2}", addr, ok, outb))
                    Next
                Else
                    Debug.WriteLine("Cart is Nothing")
                End If
            Catch ex As Exception
                Debug.WriteLine("Cart probe failed: " & ex.Message)
            End Try

            ' Clock counter quick-check
            Try
                Debug.WriteLine("Clock Counter (approx): " & ClockCounter.ToString())
            Catch
            End Try

            '    debugDumped = True
        Catch ex As Exception
            Debug.WriteLine("DebugDumpPPUState failed: " & ex.Message)
        End Try
    End Sub

End Class
