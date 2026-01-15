#pragma once

#include <cstdint>

enum class MirrorMode : uint8_t {
    Hardware = 0,
    Horizontal = 1,
    Vertical = 2,
    OneScreenLo = 3,
    OneScreenHi = 4,
    FourScreen = 5
};
