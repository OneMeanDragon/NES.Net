#pragma once
#include <cstdint>
#include "PPU2C02.Types.h"

// Handles CPU-facing register reads/writes
class PPU2C02_Registers {
protected:
    // To be called by derived class that has PpuRead/Write
    virtual uint8_t PpuRead(uint16_t addr, bool rdOnly = false) = 0;
    virtual void PpuWrite(uint16_t addr, uint8_t data) = 0;

    // OAM functions to be implemented by derived class
    virtual uint8_t ReadOAM() const = 0;
    virtual void WriteOAM(uint8_t data) = 0;
    virtual void SetOAMAddress(uint8_t addr) = 0;
protected:
    PpuControlRegister _control;
    PpuMaskRegister _mask;
    PpuStatusRegister _status;
    LoopyRegister _vramAddr;
    LoopyRegister _tramAddr;
    uint8_t _fineX = 0;
    uint8_t _addressLatch = 0;
    uint8_t _openBus = 0;

public:
    PPU2C02_Registers() = default;
    virtual ~PPU2C02_Registers() = default;

    void ResetRegisters();

    // CPU interface
    uint8_t CpuRead(uint16_t addr, bool rdOnly = false);
    void CpuWrite(uint16_t addr, uint8_t data);

    // Accessors
    const PpuControlRegister& GetControl() const { return _control; }
    const PpuMaskRegister& GetMask() const { return _mask; }
    const PpuStatusRegister& GetStatus() const { return _status; }
    LoopyRegister& GetVramAddr() { return _vramAddr; }
    LoopyRegister& GetTramAddr() { return _tramAddr; }
    uint8_t GetFineX() const { return _fineX; }
};