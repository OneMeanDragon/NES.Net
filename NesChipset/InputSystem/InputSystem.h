#pragma once

// Bus::Tick() { InputSystem::UpdateInput(); }

namespace Core {
    namespace Input {

        enum class ButtonState {
            Released = 0,
            Pressed = 1,
            Held = 2,
            JustReleased = 3
        };

        enum class MouseButton {
            Left = 0,
            Right = 1,
            Middle = 2
        };

        struct Point {
            int X;
            int Y;

            Point() : X(0), Y(0) {}
            Point(int x, int y) : X(x), Y(y) {}

            bool operator==(const Point& other) const {
                return X == other.X && Y == other.Y;
            }

            bool operator!=(const Point& other) const {
                return !(*this == other);
            }
        };

        class InputSystem {
        private:
            // Keyboard state
            static ButtonState _keyStates[256];
            static ButtonState _prevKeyStates[256];

            // Mouse state
            static ButtonState _mouseButtons[3];
            static ButtonState _prevMouseButtons[3];
            static Point _mousePosition;
            static Point _prevMousePosition;
            static int _mouseWheel;
            static int _mouseWheelDelta;

        public:
            // === Keyboard Methods ===

            /// <summary>
            /// Check if a key was just pressed this frame
            /// </summary>
            static inline bool IsKeyPressed(int key) {
                return _keyStates[key & 0xFF] == ButtonState::Pressed;
            }

            /// <summary>
            /// Check if a key is currently held down
            /// </summary>
            static inline bool IsKeyHeld(int key) {
                ButtonState state = _keyStates[key & 0xFF];
                return state == ButtonState::Held || state == ButtonState::Pressed;
            }

            /// <summary>
            /// Check if a key was just released this frame
            /// </summary>
            static inline bool IsKeyReleased(int key) {
                return _keyStates[key & 0xFF] == ButtonState::JustReleased;
            }

            /// <summary>
            /// Check if a key is in released state
            /// </summary>
            static inline bool IsKeyUp(int key) {
                ButtonState state = _keyStates[key & 0xFF];
                return state == ButtonState::Released || state == ButtonState::JustReleased;
            }

            /// <summary>
            /// Get the raw button state for a key
            /// </summary>
            static inline ButtonState GetKeyState(int key) {
                return _keyStates[key & 0xFF];
            }

            // === Mouse Button Methods ===

            /// <summary>
            /// Check if a mouse button was just pressed this frame
            /// </summary>
            static inline bool IsMousePressed(MouseButton button) {
                return _mouseButtons[static_cast<int>(button)] == ButtonState::Pressed;
            }

            /// <summary>
            /// Check if a mouse button is currently held down
            /// </summary>
            static inline bool IsMouseHeld(MouseButton button) {
                ButtonState state = _mouseButtons[static_cast<int>(button)];
                return state == ButtonState::Held || state == ButtonState::Pressed;
            }

            /// <summary>
            /// Check if a mouse button was just released this frame
            /// </summary>
            static inline bool IsMouseReleased(MouseButton button) {
                return _mouseButtons[static_cast<int>(button)] == ButtonState::JustReleased;
            }

            /// <summary>
            /// Get the raw button state for a mouse button
            /// </summary>
            static inline ButtonState GetMouseButtonState(MouseButton button) {
                return _mouseButtons[static_cast<int>(button)];
            }

            // === Mouse Position Methods ===

            /// <summary>
            /// Get the current mouse position in screen coordinates
            /// </summary>
            static inline Point GetMousePosition() {
                return _mousePosition;
            }

            /// <summary>
            /// Get the previous frame's mouse position
            /// </summary>
            static inline Point GetPreviousMousePosition() {
                return _prevMousePosition;
            }

            /// <summary>
            /// Get the mouse movement delta since last frame
            /// </summary>
            static inline Point GetMouseDelta() {
                return Point(_mousePosition.X - _prevMousePosition.X,
                    _mousePosition.Y - _prevMousePosition.Y);
            }

            /// <summary>
            /// Get mouse X coordinate
            /// </summary>
            static inline int GetMouseX() {
                return _mousePosition.X;
            }

            /// <summary>
            /// Get mouse Y coordinate
            /// </summary>
            static inline int GetMouseY() {
                return _mousePosition.Y;
            }

            /// <summary>
            /// Check if the mouse moved this frame
            /// </summary>
            static inline bool HasMouseMoved() {
                return _mousePosition != _prevMousePosition;
            }

            // === Mouse Wheel Methods ===

            /// <summary>
            /// Get the cumulative mouse wheel value
            /// </summary>
            static inline int GetMouseWheel() {
                return _mouseWheel;
            }

            /// <summary>
            /// Get the mouse wheel delta this frame (positive = up, negative = down)
            /// </summary>
            static inline int GetMouseWheelDelta() {
                return _mouseWheelDelta;
            }

            /// <summary>
            /// Check if mouse wheel scrolled up this frame
            /// </summary>
            static inline bool IsMouseWheelUp() {
                return _mouseWheelDelta > 0;
            }

            /// <summary>
            /// Check if mouse wheel scrolled down this frame
            /// </summary>
            static inline bool IsMouseWheelDown() {
                return _mouseWheelDelta < 0;
            }

            // === Update Methods ===
            static void PollKeyState();

            /// <summary>
            /// Update input state - call once per frame to poll current input state
            /// </summary>
            static void UpdateInput();

            // === Utility Methods ===

            /// <summary>
            /// Reset all input state
            /// </summary>
            static void Reset();

            /// <summary>
            /// Check if ANY key is pressed or held
            /// </summary>
            static bool IsAnyKeyPressed() {
                for (int i = 0; i < 256; i++) {
                    if (_keyStates[i] == ButtonState::Pressed ||
                        _keyStates[i] == ButtonState::Held) {
                        return true;
                    }
                }
                return false;
            }

            /// <summary>
            /// Check if ANY mouse button is pressed or held
            /// </summary>
            static bool IsAnyMouseButtonPressed() {
                for (int i = 0; i < 3; i++) {
                    if (_mouseButtons[i] == ButtonState::Pressed ||
                        _mouseButtons[i] == ButtonState::Held) {
                        return true;
                    }
                }
                return false;
            }

        private:
            static void UpdateMouseButton(int index, bool isDown) {
                if (isDown) {
                    if (_mouseButtons[index] == ButtonState::Released ||
                        _mouseButtons[index] == ButtonState::JustReleased) {
                        _mouseButtons[index] = ButtonState::Pressed;
                    }
                    else if (_mouseButtons[index] == ButtonState::Pressed) {
                        _mouseButtons[index] = ButtonState::Held;
                    }
                }
                else {
                    if (_mouseButtons[index] == ButtonState::Pressed ||
                        _mouseButtons[index] == ButtonState::Held) {
                        _mouseButtons[index] = ButtonState::JustReleased;
                    }
                    else if (_mouseButtons[index] == ButtonState::JustReleased) {
                        _mouseButtons[index] = ButtonState::Released;
                    }
                }
            }
        };

    } // namespace Input
} // namespace Core