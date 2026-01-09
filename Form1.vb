Imports System.Drawing.Imaging
Imports System.IO
Imports System.Net
Imports System.Runtime.InteropServices
Imports System.Security.Cryptography
Imports System.Threading
Imports System.Windows.Forms.VisualStyles.VisualStyleElement
Imports NAudio.FileFormats
'Audio Importing
Imports NAudio.Wave
Imports Nintendo.CoreApplicationLayer
Imports Nintendo.FOREVERLOOP_HELPERS
Imports Nintendo.GraphicsObjects
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
    Private emNES As New NintendoEntertainmentSystem.clsBus
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

    ' Modify your PlayAudioSample to include debugging
    'Private Sub PlayAudioSample(sample As Double)
    '    If Math.Abs(sample + 0.26) < 0.01 Then ' Check if it's close to the DC offset value
    '        sample = 0.0
    '    End If
    '
    '    'audioSampleCount += 1
    '
    '    '' Log first 100 samples
    '    'If audioSampleCount <= 100 Then
    '    '    Debug.WriteLine($"Sample {audioSampleCount}: {sample:F6}")
    '    'End If
    '    '
    '    '' Count non-zero samples
    '    'If Math.Abs(sample) > 0.0001 Then
    '    '    nonZeroSamples += 1
    '    'End If
    '    '
    '    '' Every 1000 samples, report status
    '    'If audioSampleCount Mod 1000 = 0 Then
    '    '    Debug.WriteLine($"Audio Stats: {audioSampleCount} total, {nonZeroSamples} non-zero ({(nonZeroSamples * 100.0 / audioSampleCount):F1}%)")
    '    '    If audioProvider IsNot Nothing Then
    '    '        Debug.WriteLine($"Buffer: {audioProvider.BufferedBytes} bytes buffered")
    '    '    End If
    '    'End If
    '    '
    '    'lastSampleValue = sample
    '    '
    '    '' Original audio code
    '    'If audioProvider Is Nothing Then
    '    '    Debug.WriteLine("WARNING: audioProvider is Nothing!")
    '    '    Return
    '    'End If
    '
    '    Dim sample16 As Int16 = CShort(Math.Max(-32768, Math.Min(32767, sample * 32767.0)))
    '    Dim bytes() As Byte = BitConverter.GetBytes(sample16)
    '    audioProvider.AddSamples(bytes, 0, 2)
    'End Sub

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
        Dim testByte As Byte = emNES.cpuRead(&HFFFCUS)
        Debug.WriteLine(String.Format("Bus.cpuRead($FFFC) = ${0:X2}", testByte))

        ' Test 2: Read $FFFC via Cart directly
        Dim cartByte As Byte = 0
        Dim cartHandled As Boolean = Cart.cpuRead(&HFFFCUS, cartByte)
        Debug.WriteLine(String.Format("Cart.cpuRead($FFFC) = handled:{0}, data:${1:X2}", cartHandled, cartByte))

        ' Test 3: Read $8000
        testByte = emNES.cpuRead(&H8000US)
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

        DrawClear(PixelColors.DARK_GREY) 'if left in the main loop screen flashes
        'Draw the Base Screen

        'Queue the patterns to be drawn
        Dim QueuePatterns As Boolean = True
        Dim QueuePalettes As Boolean = True

        Dim frameCount As Integer = 0

        InitializeAudio()
        ' Before While running
        Debug.WriteLine($"WaveOut State: {waveOut.PlaybackState}")
        Debug.WriteLine($"WaveOut Volume: {waveOut.Volume}")
        Dim audioSample As Boolean
        Dim localBatch As New List(Of Byte)
        While running

            m_timepoint2 = DateTime.Now
            Dim elapsedTime As TimeSpan = m_timepoint2 - m_timepoint1
            m_timepoint1 = m_timepoint2

            ' To get elapsed time as float in seconds (like C++ does)
            Dim elapsedSeconds As Single = CSng(elapsedTime.TotalSeconds)
            Debug.WriteLine($"Elapsed Time: {elapsedSeconds}")

            frame_start = Environment.TickCount

            emNES.PPU.frame_complete = False
            Do
                'emNES.Clock()
                audioSample = emNES.Clock()
                ClockCounter += 1

                If (ClockCounter Mod 31) = 0 Then
                    ' 1. Convert and Add to a local List(Of Byte), NOT the provider
                    Dim smp As Double = emNES.dAudioSample
                    If Math.Abs(smp + 0.26) < 0.01 Then smp = 0.0

                    Dim s16 As Int16 = CShort(Math.Max(-32768, Math.Min(32767, smp * 32767.0)))
                    localBatch.AddRange(BitConverter.GetBytes(s16))
                End If

                If running = False Then
                    emNES.Reset()
                    Exit While
                End If
            Loop While Not emNES.PPU.frame_complete

            emNES.PPU.frame_complete = False

            frameCount += 1

            If localBatch.Count > 0 Then
                audioProvider.AddSamples(localBatch.ToArray(), 0, localBatch.Count)
                localBatch.Clear()
            End If
            If waveOut.PlaybackState <> PlaybackState.Playing AndAlso audioProvider.BufferedDuration.TotalMilliseconds > 50 Then
                waveOut.Play()
            End If
            'PlayAudioSample(emNES.dAudioSample)
            'If (frameCount Mod 81) = 0 Then ' once every 81 frames..
            '    If audioSample Then
            '        ' Audio sample is ready - add it to your audio buffer
            '        PlayAudioSample(emNES.dAudioSample)
            '        'Debug.WriteLine($"WaveOut State after Play(): {emNES.dAudioSample}")
            '        'If waveOut.PlaybackState <> PlaybackState.Playing Then
            '        '    Debug.WriteLine("Starting WaveOut playback...")
            '        '    waveOut.Play()
            '        '    Threading.Thread.Sleep(100) ' Give it a moment
            '        '    Debug.WriteLine($"WaveOut State after Play(): {waveOut.PlaybackState}")
            '        'End If
            '    End If
            'End If

            ' Run diagnostic after some frames have elapsed
            'If frameCount <= 10 Then
            '    Debug.WriteLine(String.Format("Frame {0}: Mask=${1:X2}, Control=${2:X2}, VRAM=${3:X4}",
            '                      frameCount,
            '                      emNES.PPU.Debug_PPUMaskReg,
            '                      emNES.PPU.Debug_PPUControlReg,
            '                      emNES.PPU.Debug_VramReg))
            'End If
            'If frameCount <= 20 Then
            '    Dim currentPC As UInt16 = emNES.CPU.Debug_PC
            '    If currentPC = lastPC Then
            '        pcStuckCount += 1
            '        If pcStuckCount > 5 Then
            '            Debug.WriteLine(String.Format("✗ CPU STUCK at PC=${0:X4} for {1} frames!",
            '                              currentPC, pcStuckCount))
            '        End If
            '    Else
            '        If pcStuckCount > 0 Then
            '            Debug.WriteLine(String.Format("PC changed: ${0:X4} → ${1:X4} (was stuck for {2} frames)",
            '                              lastPC, currentPC, pcStuckCount))
            '        End If
            '        pcStuckCount = 0
            '        lastPC = currentPC
            '    End If
            'End If
            'If frameCount Mod 60 = 0 Then  ' Every 60 frames
            '    Debug.WriteLine("")
            '    Debug.WriteLine("=== FRAME " & frameCount & " STATUS ===")
            '    Debug.WriteLine(String.Format("PPU Control: ${0:X2} (NMI={1}, BG_enabled={2})",
            '                          emNES.PPU.Debug_PPUControlReg,
            '                          (emNES.PPU.Debug_PPUControlReg And &H80) >> 7,
            '                          (emNES.PPU.Debug_PPUControlReg And &H10) >> 4))
            '    Debug.WriteLine(String.Format("PPU Mask: ${0:X2} (Show_BG={1}, Show_SPR={2})",
            '                          emNES.PPU.Debug_PPUMaskReg,
            '                          (emNES.PPU.Debug_PPUMaskReg And &H8) >> 3,
            '                          (emNES.PPU.Debug_PPUMaskReg And &H10) >> 4))
            '    Debug.WriteLine(String.Format("VRAM addr: ${0:X4}", emNES.PPU.Debug_VramReg))
            '    Debug.WriteLine(String.Format("TRAM addr: ${0:X4}", emNES.PPU.Debug_TramReg))
            '    Debug.WriteLine(String.Format("CPU PC: ${0:X4}", emNES.CPU.Debug_PC))
            '
            '    ' Check if rendering is enabled
            '    If (emNES.PPU.Debug_PPUMaskReg And &H18) = 0 Then
            '        Debug.WriteLine("✗ WARNING: Rendering is DISABLED! (PPU Mask bits 3-4 are off)")
            '    Else
            '        Debug.WriteLine("✓ Rendering is enabled")
            '    End If
            'End If

            frame_end = Environment.TickCount

            'check PC every 10 frames:
            'If frameCount Mod 10 = 0 Then
            '    Dim currentPC As UInt16 = emNES.CPU.Debug_PC
            '    If currentPC = lastPC Then
            '        pcSameCount += 1
            '    Else
            '        pcChangeCount += 1
            '        lastPC = currentPC
            '    End If
            '
            '    If frameCount = 60 Then
            '        Debug.WriteLine(String.Format("PC Analysis: Changed {0} times, Same {1} times",
            '                          pcChangeCount, pcSameCount))
            '        If pcChangeCount < 2 Then
            '            Debug.WriteLine("✗ PC is STUCK - CPU is in infinite loop or not executing!")
            '        End If
            '    End If
            'End If
            ' Only log every 60th frame to reduce spam
            'If frameCount Mod 60 = 0 Then
            '    Debug.WriteLine(String.Format("Frame {0} completed in: {1:F3}s",
            '                             frameCount, (frame_end - frame_start) / 1000.0))
            'End If

            '// Draw rendered output ========================================================

            '-----------------------------------
            ' Draw the Patterns
            'If QueuePatterns AndAlso frameCount Mod 30 = 0 Then
            '    DrawSprite(256 + 4, 2, emNES.PPU.GetPatternTable(0, nSelectedPalette))
            '    DrawSprite(256 + 4 + (128 + 2), 2, emNES.PPU.GetPatternTable(1, nSelectedPalette))
            '    QueuePatterns = False
            'End If
            '
            'Const nSwatchSize As Integer = 6
            '' Handle palette selection changes
            'If n_PrevSelectedPallet <> nSelectedPalette Then
            '    FillRect((256 + 4) + 1 + (n_PrevSelectedPallet * (nSwatchSize * 5)), 132, (nSwatchSize * 4), nSwatchSize + 2, PixelColors.DARK_GREY)
            '    n_PrevSelectedPallet = nSelectedPalette
            '    QueuePatterns = True
            '    QueuePalettes = True
            'End If
            '
            'If QueuePalettes Then
            '    FillRect((256 + 4) + 1 + (nSelectedPalette * (nSwatchSize * 5)), 132, (nSwatchSize * 4), nSwatchSize + 2, PixelColors.CYAN)
            '    For p As Integer = 0 To 7
            '        For s As Integer = 0 To 3
            '            FillRect((256 + 4) + 1 + p * (nSwatchSize * 5) + s * nSwatchSize, 133, nSwatchSize, nSwatchSize, emNES.PPU.GetColorFromPaletteRam(p, s))
            '        Next
            '    Next
            '    QueuePalettes = False
            'End If
            '-----------------------------------

            ' Render the screen every frame
            DrawSprite(2, 2, emNES.PPU.GetScreen())

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

        If waveOut IsNot Nothing Then
            waveOut.Stop()
            waveOut.Dispose()
        End If

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
    'Private Sub DumpPPUFullDebug()
    '    If IsNothing(emNES) OrElse IsNothing(emNES.PPU) Then
    '        Debug.WriteLine("DumpPPUFullDebug: emNES.PPU is Nothing")
    '        Return
    '    End If
    '
    '    Try
    '        Debug.WriteLine("=== PPU FULL DUMP START ===")
    '        Debug.WriteLine(String.Format("PPUControl={0:X2} PPUMask={1:X2} PPUStatus={2:X2}", emNES.PPU.Debug_PPUControlReg, emNES.PPU.Debug_PPUMaskReg, emNES.PPU.Debug_PPUStatusReg))
    '        Debug.WriteLine(String.Format("vram_addr=0x{0:X4} tram_addr=0x{1:X4} fine_x={2}", emNES.PPU.Debug_VramReg, emNES.PPU.Debug_TramReg, GetType(NintendoEntertainmentSystem.em2C02).GetField("fine_x", Reflection.BindingFlags.NonPublic Or Reflection.BindingFlags.Instance)?.GetValue(emNES.PPU)))
    '
    '        ' Palette dump (32 bytes)
    '        Dim pal() As Byte = emNES.PPU.Debug_GetTblPalette()
    '        Dim sbPal As New System.Text.StringBuilder()
    '        sbPal.Append("tblPalette[0..31]:")
    '        For i As Integer = 0 To pal.Length - 1
    '            sbPal.Append(" " & pal(i).ToString("X2"))
    '        Next
    '        Debug.WriteLine(sbPal.ToString())
    '
    '        ' Dump entire nametable 0 & 1 (1024 bytes each)
    '        Dim nt0() As Byte = emNES.PPU.Debug_GetNameTableRow(0, 0, 1024)
    '        Debug.WriteLine("NameTable0 (1024 bytes):")
    '        For i As Integer = 0 To 1023 Step 32
    '            Dim line As New System.Text.StringBuilder()
    '            For j As Integer = 0 To 31
    '                line.Append(nt0(i + j).ToString("X2") & " ")
    '            Next
    '            Debug.WriteLine(line.ToString())
    '        Next
    '
    '        Dim nt1() As Byte = emNES.PPU.Debug_GetNameTableRow(1, 0, 1024)
    '        Debug.WriteLine("NameTable1 (1024 bytes):")
    '        For i As Integer = 0 To 1023 Step 32
    '            Dim line As New System.Text.StringBuilder()
    '            For j As Integer = 0 To 31
    '                line.Append(nt1(i + j).ToString("X2") & " ")
    '            Next
    '            Debug.WriteLine(line.ToString())
    '        Next
    '
    '        ' Dump attribute table bytes for name table 0 ($23C0..$23FF)
    '        Debug.WriteLine("Attribute bytes for NT0 ($23C0..$23FF):")
    '        For i As Integer = 0 To 63 Step 8
    '            Dim line As New System.Text.StringBuilder()
    '            For j As Integer = 0 To 7
    '                Dim addr As UShort = CUShort(&H23C0 + i + j)
    '                Dim b As Byte = emNES.PPU.ppuRead(addr)
    '                line.Append(b.ToString("X2") & " ")
    '            Next
    '            Debug.WriteLine(line.ToString())
    '        Next
    '
    '        ' Dump pattern bytes around tile ids likely used by title screen
    '        ' Adjust startTile/count if you want to inspect specific ids
    '        Dim startTile As Integer = 0 ' change if you know offending tile id
    '        Dim tileCount As Integer = 128
    '        Debug.WriteLine(String.Format("Pattern bytes for tiles {0}..{1} (first 16 bytes each):", startTile, startTile + tileCount - 1))
    '        For t As Integer = startTile To Math.Min(startTile + tileCount - 1, 255)
    '            Dim addrBase As UShort = CUShort(((t And &HFF) << 4)) ' assume pattern table 0 here
    '            Dim sbp As New System.Text.StringBuilder()
    '            sbp.AppendFormat("Tile {0:X2}:", t)
    '            For k As Integer = 0 To 15
    '                Dim b As Byte = emNES.PPU.ppuRead(CUShort(&H0US + addrBase + k))
    '                sbp.Append(" " & b.ToString("X2"))
    '            Next
    '            Debug.WriteLine(sbp.ToString())
    '        Next
    '
    '        Debug.WriteLine("=== PPU FULL DUMP END ===")
    '    Catch ex As Exception
    '        Debug.WriteLine("DumpPPUFullDebug failed: " & ex.Message)
    '    End Try
    'End Sub

    'Private Sub DiagnoseReset()
    '    Debug.WriteLine("=== RESET VECTOR DIAGNOSTIC ===")
    '
    '    ' Read reset vector from $FFFC/$FFFD
    '    Dim resetLow As Byte = emNES.cpuRead(&HFFFCUS)
    '    Dim resetHigh As Byte = emNES.cpuRead(&HFFFDUS)
    '    Dim resetVector As UInt16 = CUShort((resetHigh << 8) Or resetLow)
    '
    '    Debug.WriteLine(String.Format("Reset Vector: $FFFC=${0:X2}, $FFFD=${1:X2} → Start at ${2:X4}",
    '                              resetLow, resetHigh, resetVector))
    '
    '    ' Read NMI vector
    '    Dim nmiLow As Byte = emNES.cpuRead(&HFFFAUS)
    '    Dim nmiHigh As Byte = emNES.cpuRead(&HFFFBUS)
    '    Dim nmiVector As UInt16 = CUShort((nmiHigh << 8) Or nmiLow)
    '
    '    Debug.WriteLine(String.Format("NMI Vector: $FFFA=${0:X2}, $FFFB=${1:X2} → Handler at ${2:X4}",
    '                              nmiLow, nmiHigh, nmiVector))
    '
    '    ' Read IRQ vector
    '    Dim irqLow As Byte = emNES.cpuRead(&HFFFEUS)
    '    Dim irqHigh As Byte = emNES.cpuRead(&HFFFFUS)
    '    Dim irqVector As UInt16 = CUShort((irqHigh << 8) Or irqLow)
    '
    '    Debug.WriteLine(String.Format("IRQ Vector: $FFFE=${0:X2}, $FFFF=${1:X2} → Handler at ${2:X4}",
    '                              irqLow, irqHigh, irqVector))
    '
    '    ' Show first 32 bytes of reset code
    '    Debug.WriteLine("")
    '    Debug.WriteLine("First 32 bytes at reset vector:")
    '    For i As Integer = 0 To 31
    '        Dim b As Byte = emNES.cpuRead(CUShort(resetVector + i))
    '        Debug.Write(String.Format("{0:X2} ", b))
    '        If (i + 1) Mod 16 = 0 Then Debug.WriteLine("")
    '    Next
    '    Debug.WriteLine("")
    '
    '    Debug.WriteLine("=== END RESET DIAGNOSTIC ===")
    'End Sub

    'Private Sub DiagnoseNMI()
    '    ' Read NMI vector from $FFFA/$FFFB
    '    Dim nmiLow As Byte = emNES.cpuRead(&HFFFAUS)
    '    Dim nmiHigh As Byte = emNES.cpuRead(&HFFFBUS)
    '    Dim nmiVector As UInt16 = CUShort((nmiHigh << 8) Or nmiLow)
    '
    '    Debug.WriteLine(String.Format("NMI Vector: $FFFA=${0:X2}, $FFFB=${1:X2} → NMI handler at ${2:X4}",
    '                              nmiLow, nmiHigh, nmiVector))
    '
    '    ' Read first few bytes of NMI handler
    '    Debug.WriteLine("First 16 bytes of NMI handler:")
    '    For i As Integer = 0 To 15
    '        Dim b As Byte = emNES.cpuRead(CUShort(nmiVector + i))
    '        Debug.Write(String.Format("{0:X2} ", b))
    '        If (i + 1) Mod 8 = 0 Then Debug.WriteLine("")
    '    Next
    '    Debug.WriteLine("")
    'End Sub

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

    ' Update DebugDumpPPUState to use em2C02 debug accessors
    'Private Sub DebugDumpPPUState()
    '    Try
    '        Dim spr As GraphicsObjects.Sprite = emNES.PPU.GetScreen()
    '        Dim outPath As String = Path.Combine("./", "ppu_frame.png") 'Path.GetTempPath()
    '        SaveSpriteToFile(spr, outPath)
    '        Debug.WriteLine("Saved PPU frame to: " & outPath)
    '
    '        ' PPU accessors
    '        Debug.WriteLine(String.Format("PPUControl.reg = 0x{0:X2}", emNES.PPU.Debug_PPUControlReg))
    '        Debug.WriteLine(String.Format("PPUMask.reg    = 0x{0:X2}", emNES.PPU.Debug_PPUMaskReg))
    '        Debug.WriteLine(String.Format("PPUStatus.reg  = 0x{0:X2}", emNES.PPU.Debug_PPUStatusReg))
    '        Debug.WriteLine(String.Format("vram_addr.Reg = 0x{0:X4}", emNES.PPU.Debug_VramReg))
    '        Debug.WriteLine(String.Format("tram_addr.Reg = 0x{0:X4}", emNES.PPU.Debug_TramReg))
    '
    '        ' Palette & nametable via accessors
    '        Dim pal() As Byte = emNES.PPU.Debug_GetTblPalette()
    '        Dim sb As New System.Text.StringBuilder()
    '        sb.Append("tblPalette[0..31]:")
    '        For i As Integer = 0 To Math.Min(31, pal.Length - 1)
    '            sb.Append(" " & pal(i).ToString("X2"))
    '        Next
    '        Debug.WriteLine(sb.ToString())
    '
    '        Dim nt() As Byte = emNES.PPU.Debug_GetNameTableRow(0, 0, 64)
    '        Dim sbn As New System.Text.StringBuilder()
    '        sbn.Append("tblName(0)[0..63]:")
    '        For i As Integer = 0 To nt.Length - 1
    '            sbn.Append(" " & nt(i).ToString("X2"))
    '        Next
    '        Debug.WriteLine(sbn.ToString())
    '
    '        ' pattern memory copy (internal) - may be zero for CHR-ROM carts
    '        Dim pat() As Byte = emNES.PPU.Debug_GetPatternBytes(0, 0, 16)
    '        Dim sbp As New System.Text.StringBuilder()
    '        sbp.Append("tblPattern(0)[0..15]:")
    '        For i As Integer = 0 To pat.Length - 1
    '            sbp.Append(" " & pat(i).ToString("X2"))
    '        Next
    '        Debug.WriteLine(sbp.ToString())
    '
    '        ' Determine background pattern base and first tile id
    '        Dim bgPatternBase As Integer = If((emNES.PPU.Debug_PPUControlReg And &H10) <> 0, &H1000, &H0)
    '        Debug.WriteLine(String.Format("Background pattern base = 0x{0:X4}", bgPatternBase))
    '
    '        Dim firstTile As Integer = If(nt.Length > 0, nt(0), 0)
    '        Debug.WriteLine(String.Format("NameTable0 first tile id = 0x{0:X2} ({1})", firstTile, firstTile))
    '
    '        Dim tileAddr As Integer = bgPatternBase + (firstTile * 16)
    '        Debug.WriteLine(String.Format("Pattern bytes for tile 0x{0:X2} start at PPU addr 0x{1:X4}", firstTile, tileAddr))
    '
    '        ' Dump CHR around the tile to see whether cartridge has non-zero data there
    '        DumpCartCHR(tileAddr And &H1FFF, 64) ' show 64 bytes around the pattern address
    '
    '        ' Read pattern bytes via PPU.ppuRead (already done) and also call cartridge directly
    '        Dim patternViaPpu As New System.Text.StringBuilder()
    '        patternViaPpu.Append("ppuRead pattern[0..15]:")
    '        For i As Integer = 0 To 15
    '            Dim b As Byte = emNES.PPU.ppuRead(CUShort((tileAddr + i) And &H3FFFUS))
    '            patternViaPpu.Append(" " & b.ToString("X2"))
    '        Next
    '        Debug.WriteLine(patternViaPpu.ToString())
    '
    '        ' Now probe Cartridge directly (if present) to see if it answers CHR reads
    '        Try
    '            If Not IsNothing(Cart) Then
    '                Debug.WriteLine("Cart object present.")
    '                Try
    '                    Debug.WriteLine("Cart.ValidImage = " & Cart.ValidImage.ToString())
    '                Catch ex As Exception
    '                    Debug.WriteLine("Cart.ValidImage not accessible: " & ex.Message)
    '                End Try
    '
    '                For i As Integer = 0 To 15
    '                    Dim addr As UShort = CUShort((tileAddr + i) And &H3FFFUS)
    '                    Dim outb As Byte = 0
    '                    Dim ok As Boolean = False
    '                    Try
    '                        ok = Cart.ppuRead(addr, outb)
    '                    Catch ex As Exception
    '                        Debug.WriteLine("Cart.ppuRead threw: " & ex.Message)
    '                    End Try
    '                    Debug.WriteLine(String.Format("Cart.ppuRead(0x{0:X4}) returned {1}, value 0x{2:X2}", addr, ok, outb))
    '                Next
    '            Else
    '                Debug.WriteLine("Cart is Nothing")
    '            End If
    '        Catch ex As Exception
    '            Debug.WriteLine("Cart probe failed: " & ex.Message)
    '        End Try
    '
    '        ' Clock counter quick-check
    '        Try
    '            Debug.WriteLine("Clock Counter (approx): " & ClockCounter.ToString())
    '        Catch
    '        End Try
    '
    '        '    debugDumped = True
    '    Catch ex As Exception
    '        Debug.WriteLine("DebugDumpPPUState failed: " & ex.Message)
    '    End Try
    'End Sub

End Class
