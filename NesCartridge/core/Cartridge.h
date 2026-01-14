#pragma once
#include <cstdint>
#include <vector>

#include "INESHeader.h"

#define DLLEXPORT extern "C" __declspec(dllexport)

class Cartridge {
public:
    uint8_t mapperID;
    std::vector<uint8_t> prgRom;
    std::vector<uint8_t> chrRom;

    bool Load(const char* path);
    bool CpuRead(uint16_t addr, uint8_t& data);
    bool CpuWrite(uint16_t addr, uint8_t data);
};