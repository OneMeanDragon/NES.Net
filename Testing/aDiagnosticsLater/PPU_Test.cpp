#include <iostream>
#include <cstdint>
#include <cstring>
#include <cassert>
#include <vector>

// Minimal mock implementations to satisfy PPU dependencies
//enum MirrorMode {
//    Horizontal,
//    Vertical,
//    OneScreenLo,
//    OneScreenHi,
//    FourScreen
//};
enum class MirrorMode : uint8_t;
class MapperInterfaceAPI;

class MapperInterfaceAPI {
public:
    virtual void ScanlineCounter() {}
    virtual ~MapperInterfaceAPI() = default;
};

class CartridgeInterfaceAPI {
private:
    uint8_t _chrRom[8192] = { 0 };
    MirrorMode _mirrorMode = MirrorMode::Horizontal;
    MapperInterfaceAPI _mapper;

public:
    bool PpuRead(uint16_t addr, uint8_t* data) {
        if (addr < 8192) {
            *data = _chrRom[addr];
            return true;
        }
        return false;
    }

    bool PpuWrite(uint16_t addr, uint8_t data) {
        if (addr < 8192) {
            _chrRom[addr] = data;
            return true;
        }
        return false;
    }

    MirrorMode GetMirrorMode() { return _mirrorMode; }
    MapperInterfaceAPI& GetMapper() { return _mapper; }

    void SetMirrorMode(MirrorMode mode) { _mirrorMode = mode; }

    // For testing: fill pattern tables
    void FillPatternTable(uint8_t table, uint8_t value) {
        uint16_t base = table * 0x1000;
        for (int i = 0; i < 0x1000; i++) {
            _chrRom[base + i] = value;
        }
    }

    void SetPatternByte(uint16_t addr, uint8_t value) {
        if (addr < 8192) {
            _chrRom[addr] = value;
        }
    }
};