#pragma once
#include <cstdint>
#include <string>

#include "../Interfaces/MirrorModeRequired.h"

// Forward declaration
enum class MemoryRegion : uint8_t;

class MapperBase {
public:
    // Propertys
    virtual constexpr uint8_t GetMapperNumber() const noexcept = 0;
    virtual constexpr const char* GetMapperName() const noexcept = 0;
    virtual constexpr const char* GetMapperInfo() const noexcept = 0;

protected:
    uint8_t _prgBanks = 0;  // Number of 16KB PRG banks
    uint8_t _chrBanks = 0;  // Number of 8KB CHR banks

    MirrorMode _initialMirrorMode = MirrorMode::Hardware;
    MirrorMode _mirrorMode = MirrorMode::Hardware;

public:
    void SetInitalMapper(MirrorMode mirror) {
        _initialMirrorMode = mirror;
        _mirrorMode = _initialMirrorMode;
    }

public:
    // Constructor
    MapperBase(uint8_t prgBanks, uint8_t chrBanks)
        : _prgBanks(prgBanks), _chrBanks(chrBanks) {
    }

    virtual ~MapperBase() = default;

public:
    virtual void Reset() = 0;

public:
    // CPU mapping - returns true if address is handled
    // region tells cartridge which memory to access
    // mappedAddr is the offset into that memory
    virtual bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) = 0;
    virtual bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region, uint8_t data) = 0;

public:
    // PPU mapping - returns true if address is handled
    virtual bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) = 0;
    virtual bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr, MemoryRegion& region) = 0;

public:
    virtual MirrorMode GetMirrorMode() const {
        return _mirrorMode;
    }

    virtual bool IsIrqActive() const { return false; }
    virtual void ClearIrq() {}
    virtual void ScanlineCounter() {}

    uint8_t GetPrgBanks() const { return _prgBanks; }
    uint8_t GetChrBanks() const { return _chrBanks; }

};