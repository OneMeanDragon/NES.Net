Imports System.ComponentModel
Imports System.Drawing.Imaging
Imports System.IO
Imports System.Net
Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports System.Security.Cryptography
Imports System.Threading
Imports System.Windows.Forms.VisualStyles.VisualStyleElement
Imports Microsoft.VisualBasic.Devices
Imports Nintendo.Core.Input

Imports Nintendo.FOREVERLOOP_HELPERS
Imports Nintendo.NintendoEntertainmentSystem


Public Class Form1

#Region "Logging"

#Const DEBUG_LOGGING = True

    ' Then use conditional compilation:
    '#If DEBUG_LOGGING Then
    '    Debug.WriteLine("Your message")
    '#End If

    ' Or use a conditional method:
    <Conditional("DEBUG_LOGGING")>
    Private Sub LogDebug(message As String)
        Debug.WriteLine(message)
    End Sub
#End Region

#Region "Rendering"
    Private renderer As Core.Renderer

    ' Initialize renderer (call in Form1_Load after keyboard initialization)
    Private Sub InitializeRenderer()
        renderer = New Core.Renderer()
        renderer.Clear(GraphicsObjects.PixelColors.DarkGrey)
        UpdateDisplay()
    End Sub

    Private _pendingUpdate As Boolean = False

    Private Sub UpdateDisplay()
        ' Skip if there's already a pending update (don't queue up hundreds)
        If _pendingUpdate Then Return

        _pendingUpdate = True
        Dim displayBmp As Bitmap = renderer.GetDisplayBuffer()

        If picScreen.InvokeRequired Then
            picScreen.BeginInvoke(Sub()
                                      Try
                                          Dim oldImg = picScreen.Image
                                          picScreen.Image = displayBmp
                                          oldImg?.Dispose()
                                      Finally
                                          _pendingUpdate = False
                                      End Try
                                  End Sub)
        Else
            Try
                Dim oldImg = picScreen.Image
                picScreen.Image = displayBmp
                oldImg?.Dispose()
            Finally
                _pendingUpdate = False
            End Try
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

    Private Sub ProcessHotKeys()

        ' ESC to stop emulation
        If InputSystem.IsKeyHeld(Keys.Escape) Then
            running = False
        End If

        ' PrintScreen seems to come through only the KeyUp state
        If InputSystem.IsKeyHeld(Keys.F1) Then
            renderer?.SaveFrame("screenshot.png")
        End If

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
    Private resetRequest As Boolean = False
#End Region

    Public Shared running As Boolean = False

    Private ppuWriteCount As Integer = 0
    Private lastPPUWrites As New List(Of String)
    Private lastPC As UInt16 = 0
    Private pcSameCount As Integer = 0
    Private pcChangeCount As Integer = 0
    Private pcStuckCount As Integer = 0

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' This doesn't disable ALL visual styles, but helps with rendering
        Me.SetStyle(ControlStyles.AllPaintingInWmPaint Or
                ControlStyles.UserPaint Or
                ControlStyles.DoubleBuffer, True)

        ' Initialize the renderer
        InitializeInput()
        InitializeRenderer()

        Me.ClientSize = renderer?.CanvasSize
    End Sub

    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        running = False
        ' should probably wait until the thread joins back up if its still running before this
        VideoThread?.Join()
        renderer?.Dispose()
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

    Private Sub ResetToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ResetToolStripMenuItem.Click
        If running Then resetRequest = True
    End Sub

    Private Sub OpenToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenToolStripMenuItem.Click
        If running Then
            Exit Sub
        End If

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
    'Private m_timepoint1 As DateTime = DateTime.Now
    'Private m_timepoint2 As DateTime
    '-----

    Private VideoThread As Thread
    Public Sub Run()
        Dim emNES As New NativeNESBus 'NESBus
        If Not IsNothing(emNES.Cart) Then
            emNES.Cart.Dispose()
        End If
        emNES.Cart = New NativeCartridge(dlgOpenFile.FileName)
        If Not emNES.Cart.IsLoaded Then
            Return
        End If
        ' Connect our cartridge
        emNES.ConnectCartridge(emNES.Cart.NativeHandle)

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
        Dim frameCount As UInteger = 0
        Dim clocksPerFrame As ULong = 0
        Dim lastTime As DateTime = DateTime.Now
        Dim currentFps As String = ""
        Dim _frameWatch As New Stopwatch()
        Dim _fpsx = renderer.MARGIN * 2
        Dim _fpsy = renderer.MARGIN * 2
        emNES.Reset(True) ' We just inserted the cart above (first reset flips the power)
        While running
            _frameWatch.Restart()

            InputSystem.BeginFrame()
            ProcessHotKeys()

            emNES.Tick()

            ' Should be ~29780 clocks per frame (NTSC)
            'If (frameCount Mod 60) = 0 Then
            '    Debug.WriteLine($"Clocks per frame: {clocksPerFrame / 60}") 'clocksPerFrame i should export this value
            'End If

            'renderer.Clear(GraphicsObjects.PixelColors.DarkGrey)
            renderer.RenderFrame(emNES.PPU, frameCount)
            'If (DateTime.Now - lastTime).TotalSeconds >= 5.0 Then
            '    currentFps = $"FPS: {frameCount / 5}"
            '    frameCount = 0
            '    lastTime = DateTime.Now
            'End If
            ' Poor performance is why its not defaulted to on just yet
            'If _fpscounter Then renderer.DrawText(_fpsx, _fpsy, currentFps, GraphicsObjects.PixelColors.Green)
            UpdateDisplay()

            ' Diagnoses
            'CheckAudioHealth()

            'frame_ticker and reset the cycler
            frameCount += 1
            'emNES.PPU.FrameComplete = False
            While _frameWatch.Elapsed.TotalMilliseconds < 16.66667 ' 60 FPS
                'Threading.Thread.Sleep(1) ' Be more gentle than SpinWait
                Threading.Thread.SpinWait(10)
            End While
            If resetRequest Then
                resetRequest = False
                emNES.Reset(False)
            End If
            If running = False Then Exit While
        End While
        emNES.Stop() ' Fix for the always running Audio fetching {temporary}
        emNES.Dispose()

        ' Clean up
        renderer.Clear(GraphicsObjects.PixelColors.DarkGrey)
        UpdateDisplay()
        Thread.Sleep(100)
    End Sub

    Private Sub SaveCurrentFrame(filepath As String)
        renderer?.SaveFrame(filepath)
    End Sub

    Private Sub StopToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles StopToolStripMenuItem.Click
        If running Then
            running = False
            VideoThread?.Join()
        End If
    End Sub

    ' Diagnostic helpers - add inside the Form1 class

    Private debugDumped As Boolean = False
    Private framesUntilDiagnostic As Integer = -1
    Private diagnosticRun As Boolean = False

    ' Dump CHR range from the cartridge for inspection (safe, non-destructive)
    'Private Sub DumpCartCHR(startAddr As Integer, length As Integer)
    '    If IsNothing(Cart) Then
    '        Debug.WriteLine("DumpCartCHR: Cart is Nothing")
    '        Return
    '    End If
    '    Dim sb As New System.Text.StringBuilder()
    '    sb.AppendFormat("Cart CHR dump 0x{0:X4}..0x{1:X4}:", startAddr, startAddr + length - 1)
    '    Debug.WriteLine(sb.ToString())
    '    For i As Integer = 0 To length - 1
    '        Dim addr As UShort = CUShort((startAddr + i) And &H3FFFUS)
    '        Dim b As Byte = 0
    '        Dim ok As Boolean = False
    '        Try
    '            ok = Cart.PpuRead(addr, b)
    '        Catch ex As Exception
    '            Debug.WriteLine("Cart.ppuRead threw: " & ex.Message)
    '        End Try
    '        Debug.WriteLine(String.Format("  0x{0:X4}: {1} 0x{2:X2}", addr, If(ok, "OK", "NO"), b))
    '    Next
    'End Sub

    Private tmpCart As NativeCartridge

    Private Sub FpsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FpsToolStripMenuItem.Click
        'WriteConfig = Microsoft.Win32.Registry.CurrentUser.CreateSubKey(strRegistryPath)
        'ReadConfig = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(strRegistryPath)
        '
        'dlgOpenFile.InitialDirectory = ReadConfig.GetValue("LastDirectory")
        'dlgOpenFile.Filter = "NES Files (*.nes)|*.nes|All files (*.*)|*.*"
        'dlgOpenFile.FilterIndex = 0
        '
        'If dlgOpenFile.ShowDialog() = Windows.Forms.DialogResult.OK Then
        '    strFilename = Path.GetFileName(dlgOpenFile.FileName)
        '    strFilepath = Path.GetDirectoryName(dlgOpenFile.FileName)
        '    WriteConfig.SetValue("LastDirectory", strFilepath)
        '    Me.Text = strProgramTitle & " - " & strFilename
        'End If
        '
        'tmpCart = New NativeCartridge(dlgOpenFile.FileName)
    End Sub

    Private _fpscounter As Boolean = False
    Private Sub OnToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OnToolStripMenuItem.Click
        _fpscounter = True
    End Sub

    Private Sub OffToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OffToolStripMenuItem.Click
        _fpscounter = False
    End Sub
End Class
