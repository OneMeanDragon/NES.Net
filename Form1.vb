Imports System.Drawing.Imaging
Imports System.IO
Imports System.Net
Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports System.Security.Cryptography
Imports System.Threading
Imports System.Windows.Forms.VisualStyles.VisualStyleElement
Imports NAudio.FileFormats
'Audio Importing
Imports NAudio.Wave
Imports Nintendo.CoreApplicationLayer
Imports Nintendo.FOREVERLOOP_HELPERS
Imports Nintendo.NintendoEntertainmentSystem


Public Class Form1

#Region "Keyboard Mapping"
    Private Shared mapKeys As New Dictionary(Of UInteger, Byte)
    Public Shared ReadOnly Property KeyMap As Dictionary(Of UInteger, Byte)
        Get
            Return mapKeys
        End Get
    End Property
    Private Function InitalizeKeyboardMap() As ReturnCode
        mapKeys(Keys.None) = CoreApplicationLayer.Keyboard.Key.NONE
        mapKeys(Keys.A) = CoreApplicationLayer.Keyboard.Key.A
        mapKeys(Keys.B) = CoreApplicationLayer.Keyboard.Key.B
        mapKeys(Keys.C) = CoreApplicationLayer.Keyboard.Key.C
        mapKeys(Keys.D) = CoreApplicationLayer.Keyboard.Key.D
        mapKeys(Keys.E) = CoreApplicationLayer.Keyboard.Key.E
        mapKeys(Keys.F) = CoreApplicationLayer.Keyboard.Key.F
        mapKeys(Keys.G) = CoreApplicationLayer.Keyboard.Key.G
        mapKeys(Keys.H) = CoreApplicationLayer.Keyboard.Key.H
        mapKeys(Keys.I) = CoreApplicationLayer.Keyboard.Key.I
        mapKeys(Keys.J) = CoreApplicationLayer.Keyboard.Key.J
        mapKeys(Keys.K) = CoreApplicationLayer.Keyboard.Key.K
        mapKeys(Keys.L) = CoreApplicationLayer.Keyboard.Key.L
        mapKeys(Keys.M) = CoreApplicationLayer.Keyboard.Key.M
        mapKeys(Keys.N) = CoreApplicationLayer.Keyboard.Key.N
        mapKeys(Keys.O) = CoreApplicationLayer.Keyboard.Key.O
        mapKeys(Keys.P) = CoreApplicationLayer.Keyboard.Key.P
        mapKeys(Keys.Q) = CoreApplicationLayer.Keyboard.Key.Q
        mapKeys(Keys.R) = CoreApplicationLayer.Keyboard.Key.R
        mapKeys(Keys.S) = CoreApplicationLayer.Keyboard.Key.S
        mapKeys(Keys.T) = CoreApplicationLayer.Keyboard.Key.T
        mapKeys(Keys.U) = CoreApplicationLayer.Keyboard.Key.U
        mapKeys(Keys.V) = CoreApplicationLayer.Keyboard.Key.V
        mapKeys(Keys.W) = CoreApplicationLayer.Keyboard.Key.W
        mapKeys(Keys.X) = CoreApplicationLayer.Keyboard.Key.X
        mapKeys(Keys.Y) = CoreApplicationLayer.Keyboard.Key.Y
        mapKeys(Keys.Z) = CoreApplicationLayer.Keyboard.Key.Z

        mapKeys(Keys.F1) = CoreApplicationLayer.Keyboard.Key.F1
        mapKeys(Keys.F2) = CoreApplicationLayer.Keyboard.Key.F2
        mapKeys(Keys.F3) = CoreApplicationLayer.Keyboard.Key.F3
        mapKeys(Keys.F4) = CoreApplicationLayer.Keyboard.Key.F4
        mapKeys(Keys.F5) = CoreApplicationLayer.Keyboard.Key.F5
        mapKeys(Keys.F6) = CoreApplicationLayer.Keyboard.Key.F6
        mapKeys(Keys.F7) = CoreApplicationLayer.Keyboard.Key.F7
        mapKeys(Keys.F8) = CoreApplicationLayer.Keyboard.Key.F8
        mapKeys(Keys.F9) = CoreApplicationLayer.Keyboard.Key.F9
        mapKeys(Keys.F10) = CoreApplicationLayer.Keyboard.Key.F10
        mapKeys(Keys.F11) = CoreApplicationLayer.Keyboard.Key.F11
        mapKeys(Keys.F12) = CoreApplicationLayer.Keyboard.Key.F12

        mapKeys(Keys.Left) = CoreApplicationLayer.Keyboard.Key.LEFT
        mapKeys(Keys.Up) = CoreApplicationLayer.Keyboard.Key.UP
        mapKeys(Keys.Right) = CoreApplicationLayer.Keyboard.Key.RIGHT
        mapKeys(Keys.Down) = CoreApplicationLayer.Keyboard.Key.DOWN

        mapKeys(Keys.Back) = CoreApplicationLayer.Keyboard.Key.BACK

        mapKeys(Keys.Escape) = CoreApplicationLayer.Keyboard.Key.ESCAPE
        mapKeys(Keys.Enter) = CoreApplicationLayer.Keyboard.Key.ENTER

        mapKeys(Keys.Pause) = CoreApplicationLayer.Keyboard.Key.PAUSE
        mapKeys(Keys.Scroll) = CoreApplicationLayer.Keyboard.Key.SCROLL

        mapKeys(Keys.Tab) = CoreApplicationLayer.Keyboard.Key.TAB
        mapKeys(Keys.Delete) = CoreApplicationLayer.Keyboard.Key.DEL
        mapKeys(Keys.Home) = CoreApplicationLayer.Keyboard.Key.HOME

        mapKeys(Keys.End) = CoreApplicationLayer.Keyboard.Key.END
        mapKeys(Keys.PageUp) = CoreApplicationLayer.Keyboard.Key.PGUP
        mapKeys(Keys.PageDown) = CoreApplicationLayer.Keyboard.Key.PGDN
        mapKeys(Keys.Insert) = CoreApplicationLayer.Keyboard.Key.INS

        mapKeys(Keys.Shift) = CoreApplicationLayer.Keyboard.Key.SHIFT
        mapKeys(Keys.Control) = CoreApplicationLayer.Keyboard.Key.CTRL
        mapKeys(Keys.Space) = CoreApplicationLayer.Keyboard.Key.SPACE

        mapKeys(Keys.D0) = CoreApplicationLayer.Keyboard.Key.K0
        mapKeys(Keys.D1) = CoreApplicationLayer.Keyboard.Key.K1
        mapKeys(Keys.D2) = CoreApplicationLayer.Keyboard.Key.K2
        mapKeys(Keys.D3) = CoreApplicationLayer.Keyboard.Key.K3
        mapKeys(Keys.D4) = CoreApplicationLayer.Keyboard.Key.K4
        mapKeys(Keys.D5) = CoreApplicationLayer.Keyboard.Key.K5
        mapKeys(Keys.D6) = CoreApplicationLayer.Keyboard.Key.K6
        mapKeys(Keys.D7) = CoreApplicationLayer.Keyboard.Key.K7
        mapKeys(Keys.D8) = CoreApplicationLayer.Keyboard.Key.K8
        mapKeys(Keys.D9) = CoreApplicationLayer.Keyboard.Key.K9

        mapKeys(Keys.NumPad0) = CoreApplicationLayer.Keyboard.Key.NP0
        mapKeys(Keys.NumPad1) = CoreApplicationLayer.Keyboard.Key.NP1
        mapKeys(Keys.NumPad2) = CoreApplicationLayer.Keyboard.Key.NP2
        mapKeys(Keys.NumPad3) = CoreApplicationLayer.Keyboard.Key.NP3
        mapKeys(Keys.NumPad4) = CoreApplicationLayer.Keyboard.Key.NP4
        mapKeys(Keys.NumPad5) = CoreApplicationLayer.Keyboard.Key.NP5
        mapKeys(Keys.NumPad6) = CoreApplicationLayer.Keyboard.Key.NP6
        mapKeys(Keys.NumPad7) = CoreApplicationLayer.Keyboard.Key.NP7
        mapKeys(Keys.NumPad8) = CoreApplicationLayer.Keyboard.Key.NP8
        mapKeys(Keys.NumPad9) = CoreApplicationLayer.Keyboard.Key.NP9

        mapKeys(Keys.Multiply) = CoreApplicationLayer.Keyboard.Key.NP_MUL
        mapKeys(Keys.Add) = CoreApplicationLayer.Keyboard.Key.NP_ADD
        mapKeys(Keys.Divide) = CoreApplicationLayer.Keyboard.Key.NP_DIV
        mapKeys(Keys.Subtract) = CoreApplicationLayer.Keyboard.Key.NP_SUB
        mapKeys(Keys.Decimal) = CoreApplicationLayer.Keyboard.Key.NP_DECIMAL

        '// Thanks scripticuk
        mapKeys(Keys.Oem1) = CoreApplicationLayer.Keyboard.Key.OEM_1            '// On US And UK keyboards this Is the ';:' key
        mapKeys(Keys.Oem2) = CoreApplicationLayer.Keyboard.Key.OEM_2            '// On US And UK keyboards this Is the '/?' key
        mapKeys(Keys.Oem3) = CoreApplicationLayer.Keyboard.Key.OEM_3            '// On US keyboard this Is the '~' key
        mapKeys(Keys.Oem4) = CoreApplicationLayer.Keyboard.Key.OEM_4            '// On US And UK keyboards this Is the '[{' key
        mapKeys(Keys.Oem5) = CoreApplicationLayer.Keyboard.Key.OEM_5            '// On US keyboard this Is '\|' key.
        mapKeys(Keys.Oem6) = CoreApplicationLayer.Keyboard.Key.OEM_6            '// On US And UK keyboards this Is the ']}' key
        mapKeys(Keys.Oem7) = CoreApplicationLayer.Keyboard.Key.OEM_7            '// On US keyboard this Is the Single/Double quote key. On UK, this Is the Single quote/@ symbol key
        mapKeys(Keys.Oem8) = CoreApplicationLayer.Keyboard.Key.OEM_8            '// miscellaneous characters. Varies by keyboard
        mapKeys(Keys.Oemplus) = CoreApplicationLayer.Keyboard.Key.EQUALS        '// the '+' key on any keyboard
        mapKeys(Keys.Oemcomma) = CoreApplicationLayer.Keyboard.Key.COMMA        '// the comma key On any keyboard
        mapKeys(Keys.OemMinus) = CoreApplicationLayer.Keyboard.Key.MINUS        '// the minus key On any keyboard
        mapKeys(Keys.OemPeriod) = CoreApplicationLayer.Keyboard.Key.PERIOD      '// the period key On any keyboard
        mapKeys(Keys.CapsLock) = CoreApplicationLayer.Keyboard.Key.CAPS_LOCK

        Return ReturnCode.OK
    End Function
#End Region



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
    Private emNES As New NESBus
    'Private Shared emCart As New NintendoEntertainmentSystem.clsCartridge()
#End Region

#Region "Audio Setup"
    'Imports NAudio.Wave
    ' Class-level variables
    'Private waveOut As WaveOut
    Private waveOut As WaveOutEvent
    Private audioProvider As BufferedWaveProvider

    ' In your form load or initialization:
    Private Sub InitializeAudio()
        Try
            ' Set up audio output - 44.1kHz, mono, 16-bit
            Dim format As New WaveFormat(44100, 16, 1) '44100 22050
            audioProvider = New BufferedWaveProvider(format) With
                {.BufferLength = 88200, ' 2 seconds buffer
                    .DiscardOnBufferOverflow = True}

            waveOut = New WaveOutEvent()
            'waveOut = New WaveOut()
            waveOut.DeviceNumber = -1 ' = default device

            waveOut.Init(audioProvider)
            waveOut.Play()

            Debug.WriteLine("Audio initialized successfully!")
        Catch ex As Exception
            Debug.WriteLine("Audio initialization failed: " & ex.Message)
        End Try
    End Sub

    ' Add these class-level variables at the top of your form
    Private audioSampleCount As Integer = 0
    Private nonZeroSamples As Integer = 0
    Private lastSampleValue As Double = 0

    Private sampleBatch As New List(Of Byte)
    Private Sub PlayAudioSample(sample As Double)
        ' 1. Apply your silencer
        If Math.Abs(sample + 0.26) < 0.01 Then sample = 0.0

        ' 2. Convert to 16-bit
        Dim sample16 As Int16 = CShort(Math.Max(-32768, Math.Min(32767, sample * 32767.0)))
        Dim bytes() As Byte = BitConverter.GetBytes(sample16)

        ' 3. ADD TO BATCH (No locking here)
        sampleBatch.AddRange(bytes)

        ' 4. PUSH TO PROVIDER IN BULK (Only locks once per chunk)
        If sampleBatch.Count >= 1000 Then
            ' Only add if there is room to avoid "Buffer Full" crashes
            If audioProvider.BufferedBytes < (audioProvider.BufferLength - 1000) Then
                audioProvider.AddSamples(sampleBatch.ToArray(), 0, sampleBatch.Count)
            End If
            sampleBatch.Clear()
        End If
    End Sub
#End Region

    Public Shared running As Boolean = False
    Private Shared bmpBackground As Bitmap 'Figure out a way to hold that ratio

    Private ppuWriteCount As Integer = 0
    Private lastPPUWrites As New List(Of String)
    Private lastPC As UInt16 = 0
    Private pcSameCount As Integer = 0
    Private pcChangeCount As Integer = 0
    Private pcStuckCount As Integer = 0

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        If InitalizeKeyboardMap() = ReturnCode.FAIL Then
            MessageBox.Show("Failed to initalize keyboard map!", strProgramTitle, MessageBoxButtons.OK, MessageBoxIcon.Error)
            Me.Close()
            Return
        End If
        Debug.WriteLine("Keyboard map initalized successfully.")

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
        If running Then
            Exit Sub
        End If
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
        End If

        'Need to Check if were currently emulating
        If running Then Return
        If IsNothing(Cart) Then
            Cart = New CartridgeClass(dlgOpenFile.FileName)
        Else
            Cart.Reset()
            Cart.LoadFromFile(dlgOpenFile.FileName)
        End If

        If Not Cart.IsLoaded Then
            Return
        End If

        ' Reset the system
        emNES.Reset()

        '----------- debug shit
        ' Test the full read path
        Debug.WriteLine("=== FULL PATH TEST ===")

        ' Test 1: Read $FFFC via Bus
        Dim testByte As Byte = emNES.CpuRead(&HFFFCUS)
        Debug.WriteLine(String.Format("Bus.cpuRead($FFFC) = ${0:X2}", testByte))

        ' Test 2: Read $FFFC via Cart directly
        Dim cartByte As Byte = 0
        Dim cartHandled As Boolean = Cart.CpuRead(&HFFFCUS, cartByte)
        Debug.WriteLine(String.Format("Cart.cpuRead($FFFC) = handled:{0}, data:${1:X2}", cartHandled, cartByte))

        ' Test 3: Read $8000
        testByte = emNES.CpuRead(&H8000US)
        Debug.WriteLine(String.Format("Bus.cpuRead($8000) = ${0:X2}", testByte))

        Debug.WriteLine("=== END FULL PATH TEST ===")
        Debug.WriteLine("")

        ' just a ref not actual data
        ' 78 D8 A9 10 8D 00 20 A9 00 8D 01 20 8D 05 20 8D
        ' 78A9118D02804C00809AAD022010FBAD
        ' Last 16 bytes of PRG (contains vectors):
        ' ... (some bytes) ... 00 C0 82 80 F0 FF
        ' FFFFFFFFFFFFFFFFFF86C0008000C000


        ' DON'T call diagnostics here - nothing has run yet!
        ' Instead, set a flag to call it after some frames
        debugDumped = False
        framesUntilDiagnostic = 60  ' Wait 60 frames (1 second at 60fps)

        ' Start the emulation thread
        If Not IsNothing(VideoThread) Then : VideoThread = Nothing : End If
        VideoThread = New System.Threading.Thread(AddressOf Run)
        VideoThread.IsBackground = True
        running = True
        VideoThread.Start()
    End Sub
    Public Shared nSelectedPalette As UInteger = 0

    '-----
    Private m_timepoint1 As DateTime = DateTime.Now
    Private m_timepoint2 As DateTime
    '-----

    Private VideoThread As Thread
    Public Sub Run()
        '-----
        'm_timepoint1 As DateTime = DateTime.Now
        'While running
        '
        '    m_timepoint2 = DateTime.Now
        '    Dim elapsedTime As TimeSpan = m_timepoint2 - m_timepoint1
        '    m_timepoint1 = m_timepoint2
        '
        '    Dim elapsedSeconds As Single = CSng(elapsedTime.TotalSeconds)
        '    Debug.WriteLine($"Elapsed Time: {elapsedSeconds}")
        '
        'End While
        'Exit Sub
        '-----

        Dim frame_start, frame_end As Integer

        Dim n_PrevSelectedPallet As Integer = nSelectedPalette

        DrawClear(GraphicsObjects.PixelColors.DarkGrey) 'if left in the main loop screen flashes
        'Draw the Base Screen

        'Queue the patterns to be drawn
        Dim QueuePatterns As Boolean = True
        Dim QueuePalettes As Boolean = True

        Dim frameCount As Integer = 0

        'InitializeAudio()
        '' Before While running
        'Debug.WriteLine($"WaveOut State: {waveOut.PlaybackState}")
        'Debug.WriteLine($"WaveOut Volume: {waveOut.Volume}")
        'Dim audioSample As Boolean
        'Dim localBatch As New List(Of Byte)
        While running
            frame_start = Environment.TickCount

            emNES.PPU.FrameComplete = False
            Do
                emNES.Clock()

                'If (ClockCounter Mod 81) = 0 Then '31
                '    ' 1. Convert and Add to a local List(Of Byte), NOT the provider
                '    Dim smp As Double = emNES.AudioSample
                '    If Math.Abs(smp + 0.26) < 0.01 Then smp = 0.0
                '    Dim s16 As Int16 = CShort(Math.Max(-32768, Math.Min(32767, smp * 32767.0)))
                '    localBatch.AddRange(BitConverter.GetBytes(s16))
                'End If

                If running = False Then
                    emNES.Reset()
                    Exit While
                End If
            Loop While Not emNES.PPU.FrameComplete

            emNES.PPU.FrameComplete = False

            frameCount += 1UI

            'If localBatch.Count > 0 Then
            '    audioProvider.AddSamples(localBatch.ToArray(), 0, localBatch.Count)
            '    localBatch.Clear()
            'End If
            'If waveOut.PlaybackState <> PlaybackState.Playing AndAlso audioProvider.BufferedDuration.TotalMilliseconds > 50 Then
            '    waveOut.Play()
            'End If

            frame_end = Environment.TickCount

            '// Draw rendered output ========================================================
            '-----------------------------------
            ' Draw the Patterns
            If QueuePatterns AndAlso frameCount Mod 30 = 0 Then
                DrawSprite(256 + 4, 2, emNES.PPU.GetPatternTable(0, nSelectedPalette))
                DrawSprite(256 + 4 + (128 + 2), 2, emNES.PPU.GetPatternTable(1, nSelectedPalette))
                QueuePatterns = False
            End If

            Const nSwatchSize As Integer = 6
            ' Handle palette selection changes
            If n_PrevSelectedPallet <> nSelectedPalette Then
                FillRect((256 + 4) + 1 + (n_PrevSelectedPallet * (nSwatchSize * 5)), 132, (nSwatchSize * 4), nSwatchSize + 2, GraphicsObjects.PixelColors.DarkGrey)
                n_PrevSelectedPallet = nSelectedPalette
                QueuePatterns = True
                QueuePalettes = True
            End If

            If QueuePalettes Then
                FillRect((256 + 4) + 1 + (nSelectedPalette * (nSwatchSize * 5)), 132, (nSwatchSize * 4), nSwatchSize + 2, GraphicsObjects.PixelColors.Cyan)
                For p As Integer = 0 To 7
                    For s As Integer = 0 To 3
                        FillRect((256 + 4) + 1 + p * (nSwatchSize * 5) + s * nSwatchSize, 133, nSwatchSize, nSwatchSize, emNES.PPU.GetColorFromPalette(p, s))
                    Next
                Next
                QueuePalettes = False
            End If
            '-----------------------------------

            ' Render the screen every frame
            DrawSprite(2, 2, emNES.PPU.Screen)

            'Debug.WriteLine(String.Format("FPS: {0:F2}", CapTimer.CalculateFPS()))
            'Dim elapsedMs As Long = sw.ElapsedMilliseconds 'Environment.TickCount - frame_start
            'If elapsedMs < FRAMERATE_LOCK Then
            '    ' Sleep for the remaining time minus 1ms safety margin
            '    Dim sleepTime As Integer = CInt(FRAMERATE_LOCK - elapsedMs) - 1
            '    If sleepTime > 0 Then
            '        Threading.Thread.Sleep(sleepTime)
            '    End If
            'End If
        End While

        ' AUDIO CLEANUP
        'If waveOut IsNot Nothing Then
        '    waveOut.Stop()
        '    waveOut.Dispose()
        'End If

        DrawClear(GraphicsObjects.PixelColors.Black)
    End Sub

    '256=Game screen size
    '4=Extra width /2 on either side of the game screen itself
    '128+2=Full scale size of the Pattern Box + marginright and there is 2 of them
    Const BMP_WIDTH As Integer = 256 + 4 + ((128 + 2) * 2)

    '240=Game Screen height
    '4=extra space /2 above and below the screen
    Const BMP_HEIGHT As Integer = 240 + 4 '+ 800

    Private Sub DrawPixel(ByVal x As UInt32, ByVal y As UInt32, ByVal p As GraphicsObjects.Pixel)
        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT, PixelFormat.Format32bppPArgb)
        End If

        ' Convert indices to Int32 to satisfy SetPixel and avoid overflow conversions.
        Dim xi As Integer = CInt(x)
        Dim yi As Integer = CInt(y)

        ' Use explicit ARGB components to avoid relying on packed Signed integer interpretation.
        Dim col As Color = Color.FromArgb(p.A, p.R, p.G, p.B)

        bmpBackground.SetPixel(xi, yi, col)

        UpdateUI(bmpBackground)
    End Sub

    Private Sub FillRect(ByVal x As UInt32, ByVal y As UInt32, ByVal w As UInt32, ByVal h As UInt32, ByVal p As GraphicsObjects.Pixel)
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

    Private Sub DrawToScale(ByVal x As UInt32, ByVal y As UInt32, ByVal objSprite As GraphicsObjects.Sprite, Optional ByVal scale As Single = 1.0F)
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
                tempBMP.SetPixel(i, j, Color.FromArgb(objSprite.GetPixel(i, j).A, objSprite.GetPixel(i, j).R, objSprite.GetPixel(i, j).G, objSprite.GetPixel(i, j).B))
            Next
        Next
        'Resize the newly made bmp
        Dim resizedBMP As New Bitmap(tempBMP, CInt(objSprite.Width * scale), CInt(objSprite.Height * scale))
        ' Draw the new Image to the screen buffer
        If IsNothing(bmpBackground) Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT, PixelFormat.Format32bppPArgb)
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

        UpdateUI(bmpBackground)
    End Sub

    Private Sub DrawClear(ByVal p As GraphicsObjects.Pixel)
        If bmpBackground Is Nothing Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT, PixelFormat.Format32bppPArgb)
        End If

        Using g As Graphics = Graphics.FromImage(bmpBackground)
            g.Clear(Color.FromArgb(p.A, p.R, p.G, p.B))
        End Using

        UpdateUI(bmpBackground)
    End Sub


    Private Sub DrawSprite(ByVal x As Int32, ByVal y As Int32, ByVal sprite As GraphicsObjects.Sprite)
        ' 1. Lazy Initialization (Using 32bppPArgb for better performance in 2026 GDI+)
        If bmpBackground Is Nothing Then
            bmpBackground = New Bitmap(BMP_WIDTH, BMP_HEIGHT, PixelFormat.Format32bppPArgb)
        End If

        ' 2. Early Clipping Check
        Dim rectBmp As New Rectangle(0, 0, bmpBackground.Width, bmpBackground.Height)
        Dim rectSprite As New Rectangle(x, y, sprite.Width, sprite.Height)
        If Not rectBmp.IntersectsWith(rectSprite) Then Return

        ' 3. Calculate Effective Clipping Dimensions
        Dim dstX As Integer = Math.Max(0, x)
        Dim dstY As Integer = Math.Max(0, y)
        Dim srcX As Integer = If(x < 0, -x, 0)
        Dim srcY As Integer = If(y < 0, -y, 0)
        Dim copyW As Integer = Math.Min(bmpBackground.Width - dstX, sprite.Width - srcX)
        Dim copyH As Integer = Math.Min(bmpBackground.Height - dstY, sprite.Height - srcY)

        If copyW <= 0 OrElse copyH <= 0 Then Return

        ' 4. Direct Memory Blitting
        Dim bmpData As BitmapData = bmpBackground.LockBits(rectBmp, ImageLockMode.ReadWrite, bmpBackground.PixelFormat)
        Try
            ' Assuming your Sprite object has a RawData() property or pointer
            ' If it uses GetPixel(), we optimize the inner loop to avoid object creation
            Dim stride As Integer = bmpData.Stride
            Dim basePtr As IntPtr = bmpData.Scan0

            ' Optimization: Iterate rows once and use pointer arithmetic
            For row As Integer = 0 To copyH - 1
                Dim targetRowPtr As IntPtr = basePtr + ((dstY + row) * stride) + (dstX * 4)

                ' INNER LOOP: Fast Pixel Transfer
                ' We write directly to memory instead of using intermediate byte arrays
                For col As Integer = 0 To copyW - 1
                    ' Fetch pixel data once
                    Dim px As GraphicsObjects.Pixel = sprite.GetPixel(srcX + col, srcY + row)

                    ' ARGB is stored as BGRA in little-endian memory
                    Dim pxValue As Integer = (CInt(px.A) << 24) Or (CInt(px.R) << 16) Or (CInt(px.G) << 8) Or px.B
                    Marshal.WriteInt32(targetRowPtr, col * 4, pxValue)
                Next
            Next
        Finally
            bmpBackground.UnlockBits(bmpData)
        End Try

        ' 5. Efficient UI Dispatch
        ' Note: Clones are slow. In 2026, it is better to draw on a "BackBuffer" 
        ' and simply swap/invalidate the control.
        UpdateUI(bmpBackground)
    End Sub

    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Private Sub UpdateUI(ByVal sourceBmp As Bitmap)
        ' Cloned bitmap is still required if the worker thread continues immediately
        Dim displayBmp As Bitmap = DirectCast(sourceBmp.Clone(), Bitmap)

        If picScreen.InvokeRequired Then
            picScreen.BeginInvoke(Sub()
                                      Dim oldImg = picScreen.Image
                                      picScreen.Image = displayBmp
                                      oldImg?.Dispose()
                                  End Sub)
        Else
            Dim oldImg = picScreen.Image
            picScreen.Image = displayBmp
            oldImg?.Dispose()
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
                        rowBytes(off) = px.B
                        rowBytes(off + 1) = px.G
                        rowBytes(off + 2) = px.R
                        rowBytes(off + 3) = px.A
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

    Private Sub StopToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles StopToolStripMenuItem.Click
        running = False
    End Sub

    ' Diagnostic helpers - add inside the Form1 class

    Private debugDumped As Boolean = False
    Private framesUntilDiagnostic As Integer = -1
    Private diagnosticRun As Boolean = False

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

End Class
