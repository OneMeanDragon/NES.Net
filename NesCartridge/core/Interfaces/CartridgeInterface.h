#pragma once
#pragma comment(lib, "NesCartridge.lib")

#include <cstdint>

#ifdef _WIN32
#define DLLIMPORT extern "C" __declspec(dllimport)
#else
#define DLLIMPORT
#endif

// Forward declarations
class Cartridge;
class MapperBase;

// Mirroring modes (must match your Cartridge.h)
enum class MirrorMode : uint8_t {
    Hardware = 0,
    Horizontal = 1,
    Vertical = 2,
    OneScreenLo = 3,
    OneScreenHi = 4,
    FourScreen = 5
};

class MapperBase {
public:
    virtual ~MapperBase() {}
    virtual void ScanlineCounter() {}
};

// External DLL functions from your Cartridge DLL
// These should match your existing Cartridge exports
DLLIMPORT bool CartPpuRead(Cartridge* cart, uint16_t addr, uint8_t* data);
DLLIMPORT bool CartPpuWrite(Cartridge* cart, uint16_t addr, uint8_t data);
DLLIMPORT MirrorMode CartridgeGetMirrorMode(Cartridge* cart);
DLLIMPORT MapperBase* CartridgeMapper(Cartridge* cart);
DLLIMPORT void MapperScanlineCounter(MapperBase* mapper);

// Wrapper class that PPU uses to talk to Cartridge
class CartridgeInterface {
public:
    CartridgeInterface(Cartridge* cart) : _cart(cart) {}

    bool PpuRead(uint16_t addr, uint8_t& data) {
        if (!_cart) return false;
        return CartPpuRead(_cart, addr, &data);
    }

    bool PpuWrite(uint16_t addr, uint8_t data) {
        if (!_cart) return false;
        return CartPpuWrite(_cart, addr, data);
    }

    MirrorMode GetMirrorMode() const {
        if (!_cart) return MirrorMode::Hardware;
        return CartridgeGetMirrorMode(_cart);
    }

    MapperBase* GetMapper() const {
        if (!_cart) return nullptr;
        return CartridgeMapper(_cart);
    }

private:
    Cartridge* _cart;
};