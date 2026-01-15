#pragma once
/*
    This Interface should take in the option to not need the LIB linkage
	and instead use dynamic linking to call into the Cartridge DLL/SO.
*/
#pragma comment(lib, "NesCartridge.lib")

#include <core/Interfaces/MirrorModeRequired.h>

#ifdef _WIN32
#define DLLIMPORT extern "C" __declspec(dllimport)
#else
#define DLLIMPORT
#endif

// Forward declarations
class Cartridge;
class MapperBase;
enum class MirrorMode : uint8_t;

class MapperBase {
public:
    virtual ~MapperBase() {}
    virtual void ScanlineCounter() {}
};

// External DLL functions from your Cartridge DLL
// These should match your existing Cartridge exports
DLLIMPORT bool CartCpuRead(Cartridge* cart, uint16_t addr, uint8_t* data);
DLLIMPORT bool CartCpuWrite(Cartridge* cart, uint16_t addr, uint8_t data);
DLLIMPORT bool CartPpuRead(Cartridge* cart, uint16_t addr, uint8_t* data);
DLLIMPORT bool CartPpuWrite(Cartridge* cart, uint16_t addr, uint8_t data);
DLLIMPORT MirrorMode CartridgeGetMirrorMode(Cartridge* cart);
DLLIMPORT MapperBase* CartridgeMapper(Cartridge* cart);
DLLIMPORT void MapperScanlineCounter(MapperBase* mapper);
DLLIMPORT void ResetCartridge(Cartridge* cart);

// Wrapper class that PPU uses to talk to Cartridge
class CartridgeInterface {
public:
    CartridgeInterface(Cartridge* cart) : _cart(cart) {}
    virtual ~CartridgeInterface() {}

    bool CpuRead(uint16_t addr, uint8_t& data) {
        if (!_cart) return false;
        return CartCpuRead(_cart, addr, &data);
    }

    bool CpuWrite(uint16_t addr, uint8_t data) {
        if (!_cart) return false;
        return CartCpuWrite(_cart, addr, data);
    }

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

    void Reset() {
        if (!_cart) return;
        ResetCartridge(_cart);
    }

private:
    Cartridge* _cart;
};