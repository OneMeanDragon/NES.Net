Imports System.Runtime.CompilerServices
Imports System.Windows.Forms

Namespace Core.Input

    ''' <summary>
    ''' Game-engine style input system for tracking keyboard and mouse state
    ''' Provides pressed, held, and released detection for all inputs
    ''' </summary>
    Public NotInheritable Class InputSystem

#Region "Input State Enums"
        Public Enum ButtonState
            Released = 0
            Pressed = 1
            Held = 2
            JustReleased = 3
        End Enum
#End Region

#Region "Keyboard State"
        Private Shared _keyStates(255) As ButtonState
        Private Shared _prevKeyStates(255) As ButtonState
#End Region

#Region "Mouse State"
        Private Shared _mouseButtons(2) As ButtonState  ' Left, Right, Middle
        Private Shared _prevMouseButtons(2) As ButtonState
        Private Shared _mousePosition As Point
        Private Shared _prevMousePosition As Point
        Private Shared _mouseWheel As Integer
        Private Shared _mouseWheelDelta As Integer
#End Region

#Region "Keyboard Methods"
        ''' <summary>
        ''' Check if a key was just pressed this frame
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsKeyPressed(key As Keys) As Boolean
            Return _keyStates(CInt(key)) = ButtonState.Pressed
        End Function

        ''' <summary>
        ''' Check if a key is currently held down
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsKeyHeld(key As Keys) As Boolean
            Dim state = _keyStates(CInt(key))
            Return state = ButtonState.Held OrElse state = ButtonState.Pressed
        End Function

        ''' <summary>
        ''' Check if a key was just released this frame
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsKeyReleased(key As Keys) As Boolean
            Return _keyStates(CInt(key)) = ButtonState.JustReleased
        End Function

        ''' <summary>
        ''' Check if a key is in released state
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsKeyUp(key As Keys) As Boolean
            Dim state = _keyStates(CInt(key))
            Return state = ButtonState.Released OrElse state = ButtonState.JustReleased
        End Function

        ''' <summary>
        ''' Get the raw button state for a key
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function GetKeyState(key As Keys) As ButtonState
            Return _keyStates(CInt(key))
        End Function
#End Region

#Region "Mouse Button Methods"
        Public Enum MouseButton
            Left = 0
            Right = 1
            Middle = 2
        End Enum

        ''' <summary>
        ''' Check if a mouse button was just pressed this frame
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsMousePressed(button As MouseButton) As Boolean
            Return _mouseButtons(button) = ButtonState.Pressed
        End Function

        ''' <summary>
        ''' Check if a mouse button is currently held down
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsMouseHeld(button As MouseButton) As Boolean
            Dim state = _mouseButtons(button)
            Return state = ButtonState.Held OrElse state = ButtonState.Pressed
        End Function

        ''' <summary>
        ''' Check if a mouse button was just released this frame
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsMouseReleased(button As MouseButton) As Boolean
            Return _mouseButtons(button) = ButtonState.JustReleased
        End Function

        ''' <summary>
        ''' Get the raw button state for a mouse button
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function GetMouseButtonState(button As MouseButton) As ButtonState
            Return _mouseButtons(button)
        End Function
#End Region

#Region "Mouse Position Methods"
        ''' <summary>
        ''' Get the current mouse position in screen coordinates
        ''' </summary>
        Public Shared ReadOnly Property MousePosition As Point
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _mousePosition
            End Get
        End Property

        ''' <summary>
        ''' Get the previous frame's mouse position
        ''' </summary>
        Public Shared ReadOnly Property PreviousMousePosition As Point
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _prevMousePosition
            End Get
        End Property

        ''' <summary>
        ''' Get the mouse movement delta since last frame
        ''' </summary>
        Public Shared ReadOnly Property MouseDelta As Point
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return New Point(_mousePosition.X - _prevMousePosition.X,
                               _mousePosition.Y - _prevMousePosition.Y)
            End Get
        End Property

        ''' <summary>
        ''' Get mouse X coordinate
        ''' </summary>
        Public Shared ReadOnly Property MouseX As Integer
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _mousePosition.X
            End Get
        End Property

        ''' <summary>
        ''' Get mouse Y coordinate
        ''' </summary>
        Public Shared ReadOnly Property MouseY As Integer
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _mousePosition.Y
            End Get
        End Property

        ''' <summary>
        ''' Check if the mouse moved this frame
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function HasMouseMoved() As Boolean
            Return _mousePosition <> _prevMousePosition
        End Function
#End Region

#Region "Mouse Wheel Methods"
        ''' <summary>
        ''' Get the cumulative mouse wheel value
        ''' </summary>
        Public Shared ReadOnly Property MouseWheel As Integer
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _mouseWheel
            End Get
        End Property

        ''' <summary>
        ''' Get the mouse wheel delta this frame (positive = up, negative = down)
        ''' </summary>
        Public Shared ReadOnly Property MouseWheelDelta As Integer
            <MethodImpl(MethodImplOptions.AggressiveInlining)>
            Get
                Return _mouseWheelDelta
            End Get
        End Property

        ''' <summary>
        ''' Check if mouse wheel scrolled up this frame
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsMouseWheelUp() As Boolean
            Return _mouseWheelDelta > 0
        End Function

        ''' <summary>
        ''' Check if mouse wheel scrolled down this frame
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function IsMouseWheelDown() As Boolean
            Return _mouseWheelDelta < 0
        End Function
#End Region

#Region "Update Methods"
        ''' <summary>
        ''' Update input state - call once per frame BEFORE processing input
        ''' </summary>
        Public Shared Sub BeginFrame()
            ' Update keyboard states
            For i = 0 To 255
                _prevKeyStates(i) = _keyStates(i)

                ' Transition states
                Select Case _keyStates(i)
                    Case ButtonState.Pressed
                        _keyStates(i) = ButtonState.Held
                    Case ButtonState.JustReleased
                        _keyStates(i) = ButtonState.Released
                End Select
            Next

            ' Update mouse button states
            For i = 0 To 2
                _prevMouseButtons(i) = _mouseButtons(i)

                Select Case _mouseButtons(i)
                    Case ButtonState.Pressed
                        _mouseButtons(i) = ButtonState.Held
                    Case ButtonState.JustReleased
                        _mouseButtons(i) = ButtonState.Released
                End Select
            Next

            ' Update mouse position
            _prevMousePosition = _mousePosition

            ' Reset wheel delta
            _mouseWheelDelta = 0
        End Sub

        ''' <summary>
        ''' Internal: Handle key down event from WinForms
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Shared Sub HandleKeyDown(key As Keys)
            Dim keyCode = CInt(key) And 255
            If _keyStates(keyCode) = ButtonState.Released OrElse
               _keyStates(keyCode) = ButtonState.JustReleased Then
                _keyStates(keyCode) = ButtonState.Pressed
            End If
        End Sub

        ''' <summary>
        ''' Internal: Handle key up event from WinForms
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Shared Sub HandleKeyUp(key As Keys)
            Dim keyCode = CInt(key) And 255
            _keyStates(keyCode) = ButtonState.JustReleased
        End Sub

        ''' <summary>
        ''' Internal: Handle mouse down event from WinForms
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Shared Sub HandleMouseDown(button As MouseButtons)
            Dim btnIndex = GetMouseButtonIndex(button)
            If btnIndex >= 0 Then
                If _mouseButtons(btnIndex) = ButtonState.Released OrElse
                   _mouseButtons(btnIndex) = ButtonState.JustReleased Then
                    _mouseButtons(btnIndex) = ButtonState.Pressed
                End If
            End If
        End Sub

        ''' <summary>
        ''' Internal: Handle mouse up event from WinForms
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Shared Sub HandleMouseUp(button As MouseButtons)
            Dim btnIndex = GetMouseButtonIndex(button)
            If btnIndex >= 0 Then
                _mouseButtons(btnIndex) = ButtonState.JustReleased
            End If
        End Sub

        ''' <summary>
        ''' Internal: Handle mouse move event from WinForms
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Shared Sub HandleMouseMove(x As Integer, y As Integer)
            _mousePosition = New Point(x, y)
        End Sub

        ''' <summary>
        ''' Internal: Handle mouse wheel event from WinForms
        ''' </summary>
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Friend Shared Sub HandleMouseWheel(delta As Integer)
            _mouseWheel += delta
            _mouseWheelDelta = delta
        End Sub

        Private Shared Function GetMouseButtonIndex(button As MouseButtons) As Integer
            Select Case button
                Case MouseButtons.Left : Return 0
                Case MouseButtons.Right : Return 1
                Case MouseButtons.Middle : Return 2
                Case Else : Return -1
            End Select
        End Function
#End Region

#Region "Utility Methods"
        ''' <summary>
        ''' Reset all input state
        ''' </summary>
        Public Shared Sub Reset()
            Array.Clear(_keyStates, 0, _keyStates.Length)
            Array.Clear(_prevKeyStates, 0, _prevKeyStates.Length)
            Array.Clear(_mouseButtons, 0, _mouseButtons.Length)
            Array.Clear(_prevMouseButtons, 0, _prevMouseButtons.Length)
            _mousePosition = Point.Empty
            _prevMousePosition = Point.Empty
            _mouseWheel = 0
            _mouseWheelDelta = 0
        End Sub

        ''' <summary>
        ''' Get a list of all keys currently pressed or held
        ''' </summary>
        Public Shared Function GetPressedKeys() As List(Of Keys)
            Dim pressed As New List(Of Keys)
            For i = 0 To 255
                If _keyStates(i) = ButtonState.Pressed OrElse _keyStates(i) = ButtonState.Held Then
                    pressed.Add(CType(i, Keys))
                End If
            Next
            Return pressed
        End Function

        ''' <summary>
        ''' Check if ANY key is pressed or held
        ''' </summary>
        Public Shared Function IsAnyKeyPressed() As Boolean
            For i = 0 To 255
                If _keyStates(i) = ButtonState.Pressed OrElse _keyStates(i) = ButtonState.Held Then
                    Return True
                End If
            Next
            Return False
        End Function

        ''' <summary>
        ''' Check if ANY mouse button is pressed or held
        ''' </summary>
        Public Shared Function IsAnyMouseButtonPressed() As Boolean
            For i = 0 To 2
                If _mouseButtons(i) = ButtonState.Pressed OrElse _mouseButtons(i) = ButtonState.Held Then
                    Return True
                End If
            Next
            Return False
        End Function
#End Region

    End Class

End Namespace