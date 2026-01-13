Imports System.ComponentModel
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
Imports Nintendo.Core.Audio
Imports Nintendo.Core.Input

Imports Nintendo.FOREVERLOOP_HELPERS
Imports Nintendo.NintendoEntertainmentSystem


Public Class Form1

#Region "Rendering"
    Private renderer As Core.Renderer

    ' Initialize renderer (call in Form1_Load after keyboard initialization)
    Private Sub InitializeRenderer()
        renderer = New Core.Renderer()
        renderer.Clear(GraphicsObjects.PixelColors.DarkGrey)
        UpdateDisplay()
    End Sub

    Private Sub UpdateDisplay()
        Dim displayBmp As Bitmap = renderer.GetDisplayBuffer()

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
#End Region

#Region "Input System Setup"
    ' Initialize input (call in Form1_Load)
    Private Sub InitializeInput()
        ' Enable key preview so form receives key events
        Me.KeyPreview = True
        InputSystem.Reset()
        Debug.WriteLine("Input system initialized")
    End Sub

    'Direct these at the active emulator
    'nes.controller[0] |= GetKey(olc:Key : X).bHeld ? 0x80 : 0x00;     // A Button
    'nes.controller[0] |= GetKey(olc:Key : Z).bHeld ? 0x40 : 0x00;     // B Button
    'nes.controller[0] |= GetKey(olc:Key : A).bHeld ? 0x20 : 0x00;     // Select
    'nes.controller[0] |= GetKey(olc:Key : S).bHeld ? 0x10 : 0x00;     // Start
    'nes.controller[0] |= GetKey(olc:Key : UP).bHeld ? 0x08 : 0x00;
    'nes.controller[0] |= GetKey(olc:Key : DOWN).bHeld ? 0x04 : 0x00;
    'nes.controller[0] |= GetKey(olc:Key : Left).bHeld ? 0x02 : 0x00;
    'nes.controller[0] |= GetKey(olc:Key : Right).bHeld ? 0x01 : 0x00;

    Private Sub UpdateNESController()
        ' Reset controller state
        Dim controller As Byte = 0

        ' Map keyboard to NES controller
        ' WASD for D-Pad
        If InputSystem.IsKeyHeld(Keys.W) Then controller = controller Or &H8  ' Up
        If InputSystem.IsKeyHeld(Keys.S) Then controller = controller Or &H4  ' Down
        If InputSystem.IsKeyHeld(Keys.A) Then controller = controller Or &H2  ' Left
        If InputSystem.IsKeyHeld(Keys.D) Then controller = controller Or &H1  ' Right

        ' JKNM for buttons
        If InputSystem.IsKeyHeld(Keys.J) Then controller = controller Or &H20 ' Select
        If InputSystem.IsKeyHeld(Keys.K) Then controller = controller Or &H10 ' Start
        If InputSystem.IsKeyHeld(Keys.N) Then controller = controller Or &H80 ' A
        If InputSystem.IsKeyHeld(Keys.M) Then controller = controller Or &H40 ' B

        ' Update NES controller
        emNES.Controller(0) = controller
    End Sub
    'Private Sub ProcessHotkeys()
    '    ' ESC to stop emulation
    '    If InputSystem.IsKeyPressed(Keys.Escape) Then
    '        running = False
    '    End If
    '
    '    ' F1 to reset
    '    If InputSystem.IsKeyPressed(Keys.F1) Then
    '        emNES.Reset()
    '    End If
    '
    '    ' F5 to save state (when implemented)
    '    If InputSystem.IsKeyPressed(Keys.F5) Then
    '        ' SaveState()
    '        Debug.WriteLine("F5: Save state")
    '    End If
    '
    '    ' F9 to load state (when implemented)
    '    If InputSystem.IsKeyPressed(Keys.F9) Then
    '        ' LoadState()
    '        Debug.WriteLine("F9: Load state")
    '    End If
    '
    '    ' P to cycle palettes
    '    If InputSystem.IsKeyPressed(Keys.P) Then
    '        renderer.SelectedPalette = (renderer.SelectedPalette + 1) Mod 8
    '    End If
    '
    '    ' Ctrl+M to mute audio
    '    If InputSystem.IsKeyHeld(Keys.LControlKey) AndAlso
    '   InputSystem.IsKeyPressed(Keys.M) Then
    '        audioSystem.Volume = If(audioSystem.Volume > 0, 0.0F, 0.5F)
    '    End If
    '
    '    ' +/- for volume
    '    If InputSystem.IsKeyPressed(Keys.Oemplus) OrElse InputSystem.IsKeyPressed(Keys.Add) Then
    '        audioSystem.Volume = Math.Min(1.0F, audioSystem.Volume + 0.1F)
    '    End If
    '    If InputSystem.IsKeyPressed(Keys.OemMinus) OrElse InputSystem.IsKeyPressed(Keys.Subtract) Then
    '        audioSystem.Volume = Math.Max(0.0F, audioSystem.Volume - 0.1F)
    '    End If
    'End Sub
    ' Add ProcessHotkeys() call to your Run() loop after BeginFrame()
#End Region

#Region "Audio System"
    Private audioSystem As AudioSystem

    ' Initialize audio system (call in Form1_Load)
    Private Sub InitializeAudio()
        audioSystem = New AudioSystem()
        If audioSystem.Initialize(44100) Then
            audioSystem.Volume = 0.5F  ' 50% volume
            audioSystem.LowPassFilterStrength = 0.9 '85 ' Default, good balance
            audioSystem.LowPassFilterEnabled = False ' Disable filter if you want raw audio
            Debug.WriteLine("Audio system initialized successfully")
        Else
            MessageBox.Show("Failed to initialize audio system. Audio will be disabled.",
                          strProgramTitle, MessageBoxButtons.OK, MessageBoxIcon.Warning)
        End If
    End Sub
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
#End Region

    Public Shared running As Boolean = False

    Private ppuWriteCount As Integer = 0
    Private lastPPUWrites As New List(Of String)
    Private lastPC As UInt16 = 0
    Private pcSameCount As Integer = 0
    Private pcChangeCount As Integer = 0
    Private pcStuckCount As Integer = 0

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Initialize the renderer
        InitializeInput()
        InitializeRenderer()
        InitializeAudio()
    End Sub
    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        running = False
        ' should probably wait until the thread joins back up if its still running before this 
        renderer?.Dispose()
        audioSystem?.Dispose()
    End Sub

    Private Sub Form1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles Me.KeyPress
        If e.KeyChar = "p"c Then
            renderer.SelectedPalette = (renderer.SelectedPalette + 1) Mod 8
        End If
    End Sub

    Private Sub Form1_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        InputSystem.HandleKeyDown(e.KeyCode)
        e.Handled = True
    End Sub

    Private Sub Form1_KeyUp(sender As Object, e As KeyEventArgs) Handles Me.KeyUp
        InputSystem.HandleKeyUp(e.KeyCode)
        e.Handled = True
    End Sub

    Private Sub Form1_MouseDown(sender As Object, e As MouseEventArgs) Handles Me.MouseDown
        InputSystem.HandleMouseDown(e.Button)
    End Sub

    Private Sub Form1_MouseUp(sender As Object, e As MouseEventArgs) Handles Me.MouseUp
        InputSystem.HandleMouseUp(e.Button)
    End Sub

    Private Sub Form1_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        InputSystem.HandleMouseMove(e.X, e.Y)
    End Sub

    Private Sub Form1_MouseWheel(sender As Object, e As MouseEventArgs) Handles Me.MouseWheel
        InputSystem.HandleMouseWheel(e.Delta)
    End Sub

    Private Sub OpenToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenToolStripMenuItem.Click
        If running Then
            Exit Sub
        End If

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
        Dim frameCount As Integer = 0
        While running
            InputSystem.BeginFrame()
            UpdateNESController()

            emNES.PPU.FrameComplete = False
            Do
                emNES.Clock()

                If audioSystem IsNot Nothing AndAlso audioSystem.IsInitialized Then
                    ' Check if audio sample is ready (you'll need to implement this in your bus)
                    If emNES.AudioSampleReady Then
                        audioSystem.ProcessSample(emNES.AudioSample)
                    End If
                End If

                If running = False Then
                    emNES.Reset()
                    Exit While
                End If
            Loop While Not emNES.PPU.FrameComplete

            emNES.PPU.FrameComplete = False

            frameCount += 1UI

            ' Render frame using the renderer
            renderer.RenderFrame(emNES.PPU, frameCount)

            ' Update display
            UpdateDisplay()

            ' Flush audio batch at frame boundary (optional - helps with timing)
            audioSystem?.Flush()
        End While

        ' Clean up
        audioSystem?.Flush()
        audioSystem?.Stop()
        renderer.Clear(GraphicsObjects.PixelColors.DarkGrey)
        UpdateDisplay()
    End Sub

    Private Sub SaveCurrentFrame(filepath As String)
        renderer?.SaveFrame(filepath)
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
                ok = Cart.PpuRead(addr, b)
            Catch ex As Exception
                Debug.WriteLine("Cart.ppuRead threw: " & ex.Message)
            End Try
            Debug.WriteLine(String.Format("  0x{0:X4}: {1} 0x{2:X2}", addr, If(ok, "OK", "NO"), b))
        Next
    End Sub

End Class
