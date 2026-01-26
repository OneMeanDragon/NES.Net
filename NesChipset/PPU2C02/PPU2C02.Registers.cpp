#include "PPU2C02.Registers.h"
#include <iostream>

void PPU2C02_Registers::ResetRegisters() {
    _control.reg = 0;
    _mask.reg = 0;
    _status.reg = 0xA0;
    _vramAddr.reg = 0x2000;
    _tramAddr.reg = 0;
    _fineX = 0;
    _addressLatch = 0;
    _openBus = 0;
    _dataBuffer = 0;
}

uint8_t PPU2C02_Registers::CpuRead(uint16_t addr, bool rdOnly) {
    // Normal read mode - has side effects
    switch (addr & 0xF) {
    case 0x0000: return _openBus;// PPUCTRL   ($2000) - Write-only
    case 0x0001: return _openBus;// PPUMASK   ($2001) - Write-only
    case 0x0003: return _openBus;// OAMADDR   ($2003) - Write-only
    case 0x0005: return _openBus;// PPUSCROLL ($2005) - Write-only
    case 0x0006: return _openBus;// PPUADDR   ($2006) - Write-only

    case 0x0002: {// PPUSTATUS ($2002) - Read-only
        uint8_t value = (_status.reg & 0xE0) | (_openBus & 0x1F);
        _status.verticalBlank = 0;  // clear VBL flag on read
        _addressLatch = 0;           // reset latch
        _openBus = value;
        return value;
    }
    case 0x0004: {// OAMDATA ($2004) - Read/Write
        _openBus = ReadOAM();
        return _openBus;
    }
    case 0x0007: {// PPUDATA ($2007) - Read/Write
        uint16_t vAddr = _vramAddr.reg;
        uint8_t value = _dataBuffer;

        // Read from PPU memory
        _dataBuffer = PpuRead(vAddr);

        // Palette memory reads bypass the buffer
        if (vAddr >= 0x3F00) {
            value = _dataBuffer;
        }

        // Increment VRAM address
        //_vramAddr.reg += (_control.incrementMode ? 32 : 1);
        _vramAddr.reg = (_vramAddr.reg + (_control.incrementMode ? 32 : 1)) & 0x3FFF;
        _openBus = value;
        return value;
    }

    default: return _openBus;
    }

    return 0x00;
}

void PPU2C02_Registers::CpuWrite(uint16_t addr, uint8_t data) {
    _openBus = data & 0xFF;

    switch (addr & 0xf) {
    case 0x0000: // PPUCTRL ($2000)
        _control.reg = data;
        _tramAddr.SetNametableX(_control.nametableX);
        _tramAddr.SetNametableY(_control.nametableY);
        break;

    case 0x0001: // PPUMASK ($2001)
        _mask.reg = data;
        break;

    case 0x0002: return; // PPUSTATUS ($2002) // Read-only register, writes are ignored

    case 0x0003: // OAMADDR ($2003)
        SetOAMAddress(data);
        break;

    case 0x0004: // OAMDATA ($2004)
        WriteOAM(data);
        break;

    case 0x0005: // PPUSCROLL ($2005)
        if (_addressLatch == 0) {
            _tramAddr.SetCoarseX(data >> 3);
            _fineX = data & 0x07;
            _addressLatch = 1;
            _tramAddr.reg &= 0x0FFF;  // CLEAR upper bits after scroll write
            //printf("PPUSCROLL X: data=%02X tramAddr=%04X\n", data, _tramAddr.reg);
        }
        else {
            _tramAddr.SetCoarseY(data >> 3);
            _tramAddr.SetFineY(data & 0x07);
            _addressLatch = 0;
            _tramAddr.reg &= 0x7FFF;  //  CLEAR bit 15
            //printf("PPUSCROLL Y: data=%02X tramAddr=%04X\n", data, _tramAddr.reg);
        }
        break;

    case 0x0006: // PPUADDR ($2006)
        if (_addressLatch == 0) {
            _tramAddr.reg = (_tramAddr.reg & 0x00FF) | ((data & 0x3F) << 8);
            _addressLatch = 1;
            //printf("PPUADDR hi: data=%02X tramAddr=%04X\n", data, _tramAddr.reg);
        }
        else {
            _tramAddr.reg = (_tramAddr.reg & 0xFF00) | data;
            _vramAddr.reg = _tramAddr.reg & 0x3FFF;
            _addressLatch = 0;
            //printf("PPUADDR lo: data=%02X vramAddr=%04X tramAddr=%04X\n",
            //    data, _vramAddr.reg, _tramAddr.reg);
        }
        break;

    case 0x0007: // PPUDATA ($2007)
        PpuWrite(_vramAddr.reg & 0x3FFF, data);
        _vramAddr.reg += (_control.incrementMode ? 32 : 1) & 0x3FFF;
        break;
    }
}