#include <cstring>
#include "InputSystem.h"

namespace Core::Input
{
    ButtonState InputSystem::_keyStates[256] = {};
    ButtonState InputSystem::_prevKeyStates[256] = {};
    ButtonState InputSystem::_mouseButtons[3] = {};
    ButtonState InputSystem::_prevMouseButtons[3] = {};
    Point InputSystem::_mousePosition = Point();
    Point InputSystem::_prevMousePosition = Point();
    int InputSystem::_mouseWheel = 0;
    int InputSystem::_mouseWheelDelta = 0;

    void InputSystem::Reset() {
        memset(_keyStates, 0, sizeof(_keyStates));
        memset(_prevKeyStates, 0, sizeof(_prevKeyStates));
        memset(_mouseButtons, 0, sizeof(_mouseButtons));
        memset(_prevMouseButtons, 0, sizeof(_prevMouseButtons));
        _mousePosition = Point();
        _prevMousePosition = Point();
        _mouseWheel = 0;
        _mouseWheelDelta = 0;
    }

    void InputSystem::UpdateInput() {
        // Update previous states
        memcpy(_prevKeyStates, _keyStates, sizeof(_keyStates));
        memcpy(_prevMouseButtons, _mouseButtons, sizeof(_mouseButtons));
        _prevMousePosition = _mousePosition;

        // Reset wheel delta
        _mouseWheelDelta = 0;

        PollKeyState();
    }
}