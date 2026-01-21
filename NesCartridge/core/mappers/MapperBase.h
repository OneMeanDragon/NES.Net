#pragma once
#include <cstdint>
#include <string>
#include <vector>

#include "../Interfaces/MirrorModeRequired.h"

class MapperBase {
public: // Propertys
    virtual constexpr uint8_t GetMapperNumber() const noexcept = 0;
    virtual constexpr const char* GetMapperName() const noexcept = 0;
protected:
    uint8_t _prgBanks = 0;
    uint8_t _chrBanks = 0;
    MirrorMode _mirrorMode = MirrorMode::Hardware;
    std::vector<uint8_t> _cartRam{};

public:
    // Constructor
    MapperBase(uint8_t prgBanks, uint8_t chrBanks)
        : _prgBanks(prgBanks), _chrBanks(chrBanks), _mirrorMode(MirrorMode::Hardware) {
    }

    virtual ~MapperBase() = default;

public:
    virtual void Reset() = 0;

public:
    virtual bool CpuMapRead(uint16_t addr, uint32_t& mappedAddr, uint8_t& data) = 0;
    virtual bool CpuMapWrite(uint16_t addr, uint32_t& mappedAddr, uint8_t data) = 0;
public:
    virtual bool PpuMapRead(uint16_t addr, uint32_t& mappedAddr) = 0;
    virtual bool PpuMapWrite(uint16_t addr, uint32_t& mappedAddr) = 0;
public:
    virtual MirrorMode GetMirrorMode() const {
        return _mirrorMode;
    }

    virtual bool IsIrqActive() const { return false; }
    virtual void ClearIrq() {}
    virtual void ScanlineCounter() {}

    uint8_t GetPrgBanks() const { return _prgBanks; }
    uint8_t GetChrBanks() const { return _chrBanks; }

public:
    void Clock() {}
};
