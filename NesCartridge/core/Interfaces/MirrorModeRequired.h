#pragma once

#include <cstdint>

#if defined(RELEASE_MODE)
enum class MirrorMode : uint8_t {
    Hardware = 0,
    Horizontal = 1,
    Vertical = 2,
    OneScreenLo = 3,
    OneScreenHi = 4,
    FourScreen = 5
};
#else
enum MirrorMode {
    Hardware = 0,
    Horizontal = 1,
    Vertical = 2,
    OneScreenLo = 3,
    OneScreenHi = 4,
    FourScreen = 5
};
#endif
