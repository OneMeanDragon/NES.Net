#if !defined(_WIN32)
    #include "InputSystem.h"
#else
#include <windows.h>
#include "InputSystem.h"

namespace Core::Input
{

    void InputSystem::PollKeyState() {
        // Poll keyboard state using GetAsyncKeyState
        for (int i = 0; i < 256; i++) {
            bool isDown = (GetAsyncKeyState(i) & 0x8000) != 0;

            if (isDown) {
                // Key is currently down
                if (_keyStates[i] == ButtonState::Released ||
                    _keyStates[i] == ButtonState::JustReleased) {
                    _keyStates[i] = ButtonState::Pressed;
                }
                else if (_keyStates[i] == ButtonState::Pressed) {
                    _keyStates[i] = ButtonState::Held;
                }
            }
            else {
                // Key is currently up
                if (_keyStates[i] == ButtonState::Pressed ||
                    _keyStates[i] == ButtonState::Held) {
                    _keyStates[i] = ButtonState::JustReleased;
                }
                else if (_keyStates[i] == ButtonState::JustReleased) {
                    _keyStates[i] = ButtonState::Released;
                }
            }
        }

        // Poll mouse buttons
        bool leftDown = (GetAsyncKeyState(VK_LBUTTON) & 0x8000) != 0;
        bool rightDown = (GetAsyncKeyState(VK_RBUTTON) & 0x8000) != 0;
        bool middleDown = (GetAsyncKeyState(VK_MBUTTON) & 0x8000) != 0;

        UpdateMouseButton(0, leftDown);
        UpdateMouseButton(1, rightDown);
        UpdateMouseButton(2, middleDown);

        // Poll mouse position
        POINT cursorPos;
        if (GetCursorPos(&cursorPos)) {
            _mousePosition.X = cursorPos.x;
            _mousePosition.Y = cursorPos.y;
        }
    }

}

#endif