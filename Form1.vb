Imports System.IO
Imports System.Threading

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
        Dim frame_start, frame_end As UInt32

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
            emNES.PPU.frame_complete = False
            Do
                emNES.Clock()
                ClockCounter += 1
                If running = False Then
                    emNES.Reset()
                    Exit While
                    Exit Do
                End If
            Loop While Not emNES.PPU.frame_complete
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
                ' DrawToScale(256 + 4, 2, emNES.PPU.GetPatternTable(0, nSelectedPalette), 0.5)        'Also working, bitmap scaleing sucks though lol 0.5 = (128x128)=64x64
                ' DrawToScale(256 + 6 + 64, 2, emNES.PPU.GetPatternTable(1, nSelectedPalette), 0.5)   'Also working, bitmap scaleing sucks though lol 0.5 = (128x128)=64x64
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

            ' Render the screen
            DrawSprite(2, 2, emNES.PPU.GetScreen()) 'not working


            '----------------------
            'For i As Integer = 0 To 4
            '    For j As Integer = 0 To 4
            '        Dim id As Byte = emNES.PPU.tblName(0, i * 32 + j)
            '        DrawSprite(i * 16, j * 16, emNES.PPU.GetPatternTable(0, nSelectedPalette))
            '    Next
            'Next



            Debug.WriteLine("FPS:" & CapTimer.CalculateFPS().ToString())
            'Sleep Frames or not (Not needed ever since adding the draw procedure, was running 900FPS without it, now its lucky to make 3FPS at times lol, though my calculation in the timer is likely wrong aswell)
            'Dim frameticks As UInteger = CapTimer.GetDelta()
            'If frameticks < FRAMERATE_LOCK Then 
            '    Threading.Thread.Sleep(FRAMERATE_LOCK - frameticks)
            'End If
        End While
        DrawClear(PixelColors.DARK_BLUE)
    End Sub

    '256=Game screen size
    '4=Extra width /2 on either side of the game screen itself
    '128+2=Full scale size of the Pattern Box + marginright and there is 2 of them
    Const BMP_WIDTH As Integer = 256 + 4 + ((128 + 2) * 2)

    '240=Game Screen height
    '4=extra space /2 above and below the screen
    Const BMP_HEIGHT As Integer = 240 + 4 '+ 800

    Private Sub DrawPixel(ByVal x As UInt32, ByVal y As UInt32, ByVal p As Pixel)
        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT)
        End If
        bmpBackground.SetPixel(x, y, Color.FromArgb(p.m_Pixel.Signed))
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
                DrawSprite(x, y, objSprite)
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
            For j As Integer = 0 To tempBMP.Width - 1
                tempBMP.SetPixel(i, j, Color.FromArgb(objSprite.GetPixel(i, j).m_Pixel.Signed))
            Next
        Next
        'Resize the newly made bmp
        Dim resizedBMP As New Bitmap(tempBMP, (objSprite.Width * scale), (objSprite.Height * scale))
        ' Draw the new Image to the screen buffer
        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT)
        End If

        For i As Int32 = 0 To resizedBMP.Width - 1
            For j As Int32 = 0 To resizedBMP.Height - 1
                bmpBackground.SetPixel(x + i, y + j, resizedBMP.GetPixel(i, j))
            Next
        Next

        'Dispose our shit
        tempBMP.Dispose()
        resizedBMP.Dispose()

        If picScreen.InvokeRequired Then
            picScreen.Invoke(New DoStuffDelegate(AddressOf picScreenDel), bmpBackground)
        Else
            picScreen.Image = bmpBackground
            picScreen.Refresh()
        End If
    End Sub

#Region "bNES DrawScreen [dosent work here]"
    'Private Sub TestScreenDraw(ByVal x As UInt32, ByVal y As UInt32)
    '    If IsNothing(bmpBackground) Then
    '        Dim extra_width_for_palettes_etc As Integer = 134
    '        bmpBackground = New Bitmap(256 + extra_width_for_palettes_etc, 240 + 28)
    '    End If

    '    Dim VideoLine As Integer
    '    Dim LinePixel As Integer
    '    For VideoLine = 0 To 239
    '        For LinePixel = 0 To 255
    '            'Dim Value As Byte = PPU.vBuffer(VideoLine * 256 + LinePixel)
    '            Dim Value As Byte = emNES.PPU.ppuRead(VideoLine * 256 + LinePixel)
    '            Select Case Value
    '                Case &H4, &H8, &HC, &H10, &H14, &H18, &H1C : Value = 0
    '            End Select
    '            'bmpBackground.SetPixel(LinePixel, VideoLine, SYS.NESColor(VRAM.Read(&H3F00 + Value)))
    '            bmpBackground.SetPixel(LinePixel, VideoLine, Color.FromArgb(emNES.PPU.palScreen(emNES.PPU.ppuRead(&H3F00 + Value)).m_Pixel.Signed))
    '            'If ShowGrid Then
    '            '    If VideoLine Mod 8 = 0 And LinePixel Mod 2 = 0 Then bmpBackground.SetPixel(LinePixel, VideoLine, Color.White)
    '            '    If VideoLine Mod 2 = 0 And LinePixel Mod 8 = 0 Then bmpBackground.SetPixel(LinePixel, VideoLine, Color.White)
    '            '    If VideoLine Mod 32 = 0 Or LinePixel Mod 32 = 0 Then bmpBackground.SetPixel(LinePixel, VideoLine, Color.White)
    '            'End If
    '        Next
    '    Next
    'End Sub
#End Region

    Private Sub DrawClear(ByVal p As Pixel)

        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT)
        End If

        For i As Int32 = 0 To bmpBackground.Width - 1
            For j As Int32 = 0 To bmpBackground.Height - 1
                bmpBackground.SetPixel(i, j, Color.FromArgb(p.m_Pixel.Signed))
            Next
        Next

        'forgot to post the dead background after ending emulation
        If picScreen.InvokeRequired Then
            picScreen.Invoke(New DoStuffDelegate(AddressOf picScreenDel), bmpBackground)
        Else
            picScreen.Image = bmpBackground
            picScreen.Refresh()
        End If

    End Sub

    Private Sub DrawSprite(ByVal x As Int32, ByVal y As Int32, ByVal ImageObj As GraphicsObjects.Sprite)
        'fx = fxs;
        'For (int32_t i = 0; i < sprite->width; i++, fx += fxm)
        '{
        '	fy = fys;
        '	For (int32_t j = 0; j < sprite->height; j++, fy += fym)
        '		Draw(x + i, y + j, sprite -> GetPixel(fx, fy));
        '}

        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT)
        End If

        For i As Int32 = 0 To ImageObj.Width - 1
            For j As Int32 = 0 To ImageObj.Height - 1
                bmpBackground.SetPixel(x + i, y + j, Color.FromArgb(ImageObj.GetPixel(i, j).m_Pixel.Signed))
            Next
        Next

        If picScreen.InvokeRequired Then
            picScreen.Invoke(New DoStuffDelegate(AddressOf picScreenDel), bmpBackground)
        Else
            picScreen.Image = bmpBackground
            picScreen.Refresh()
        End If

    End Sub
    Private Delegate Sub DoStuffDelegate(bg As Bitmap)
    Sub picScreenDel(bg As Bitmap)
        picScreen.Image = bg
        picScreen.Refresh()
    End Sub

    Private Sub StopToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles StopToolStripMenuItem.Click
        running = False
    End Sub

End Class
