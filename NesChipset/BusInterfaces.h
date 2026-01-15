#pragma once

#include <cstdint>

#ifdef _WIN32
#define DLLIMPORT extern "C" __declspec(dllimport)
#else
#define DLLIMPORT
#endif

// Forward declarations
//class Cartridge;
//class PPU2C02;
//class CPU6502;
//class APU2A03;
//class MapperBase;

// Cartridge DLL imports (from your existing Cartridge DLL)
DLLIMPORT bool CartCpuRead(class Cartridge* cart, uint16_t addr, uint8_t* data);
DLLIMPORT bool CartCpuWrite(class Cartridge* cart, uint16_t addr, uint8_t data);
DLLIMPORT class MapperBase* CartridgeMapper(class Cartridge* cart);
DLLIMPORT void ResetCartridge(class Cartridge* cart);
DLLIMPORT bool MapperIsIrqActive(class MapperBase* mapper);
DLLIMPORT void MapperClearIrq(class MapperBase* mapper);

// PPU DLL imports (from your PPU DLL)
DLLIMPORT void PPU_Clock(class PPU2C02* ppu);
DLLIMPORT uint8_t PPU_CpuRead(class PPU2C02* ppu, uint16_t addr, bool rdOnly);
DLLIMPORT void PPU_CpuWrite(class PPU2C02* ppu, uint16_t addr, uint8_t data);
DLLIMPORT void PPU_SetOAMByte(class PPU2C02* ppu, uint8_t oamAddr, uint8_t data);
DLLIMPORT bool PPU_GetNmiRequested(class PPU2C02* ppu);
DLLIMPORT void PPU_ClearNmiRequested(class PPU2C02* ppu);
DLLIMPORT void PPU_Reset(class PPU2C02* ppu);

// CPU DLL imports (placeholder - you'll create these when you port CPU)
// For now, we'll just declare them and they'll link when CPU DLL is ready
//DLLIMPORT void ClockCPU(class CPU6502* cpu);
//DLLIMPORT void ResetCPU(class CPU6502* cpu);
//DLLIMPORT void TriggerNMI(class CPU6502* cpu);
//DLLIMPORT void TriggerIRQ(class CPU6502* cpu);

// APU DLL imports (placeholder - if you port APU later)
DLLIMPORT void ClockAPU(class APU2A03* apu);
DLLIMPORT void ResetAPU(class APU2A03* apu);
DLLIMPORT uint8_t APU_CpuRead(class APU2A03* apu, uint16_t addr);
DLLIMPORT void APU_CpuWrite(class APU2A03* apu, uint16_t addr, uint8_t data);
DLLIMPORT double APU_GetOutputSample(class APU2A03* apu);