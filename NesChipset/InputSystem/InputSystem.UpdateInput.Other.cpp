#if defined(_WIN32)
#include "InputSystem.h"
#else
#include <cstring>
#include "InputSystem.h"

namespace Core::Input
{

	void InputSystem::PollKeyState() {
        // For non-Windows platforms, transition existing states
        // (You'll need to implement platform-specific polling or use a library)
        for (int i = 0; i < 256; i++) {
            switch (_keyStates[i]) {
            case ButtonState::Pressed:
                _keyStates[i] = ButtonState::Held;
                break;
            case ButtonState::JustReleased:
                _keyStates[i] = ButtonState::Released;
                break;
            }
        }

        for (int i = 0; i < 3; i++) {
            switch (_mouseButtons[i]) {
            case ButtonState::Pressed:
                _mouseButtons[i] = ButtonState::Held;
                break;
            case ButtonState::JustReleased:
                _mouseButtons[i] = ButtonState::Released;
                break;
            }
        }
    }

}

#endif