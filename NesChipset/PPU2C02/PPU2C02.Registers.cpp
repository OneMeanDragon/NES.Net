#include "PPU2C02.Registers.h"

void PPU2C02_Registers::ResetRegisters() {
    _control.reg = 0;
    _mask.reg = 0;
    _status.reg = 0xA0;
    _vramAddr.reg = 0x2000;
    _tramAddr.reg = 0;
    _fineX = 0;
    _addressLatch = 0;
    _openBus = 0;
}

uint8_t PPU2C02_Registers::CpuRead(uint16_t addr, bool rdOnly) {
    uint8_t data = 0x00;

    if (rdOnly) {
        // Read-only mode for debugger/tools - no side effects
        switch (addr) {
        case 0x0000: data = _control.reg; break;
        case 0x0001: data = _mask.reg; break;
        case 0x0002: data = _status.reg; break;
        case 0x0007: data = PpuRead(_vramAddr.reg); break;
        }
    }
    else {
        // Normal read mode - has side effects
        switch (addr) {
        case 0x0000: // PPUCTRL   ($2000) - Write-only
        case 0x0001: // PPUMASK   ($2001) - Write-only
        case 0x0005: // PPUSCROLL ($2005) - Write-only
        case 0x0006: // PPUADDR   ($2006) - Write-only
            data = _openBus;
            break;

        case 0x0002: // PPUSTATUS ($2002) - Read-only
            data = (_status.reg & 0xE0) | (_openBus & 0x1F);
            _status.verticalBlank = false;
            _addressLatch = 0;
            break;

        case 0x0007: {// PPUDATA ($2007) - Read/Write
            uint8_t buffer = PpuRead(_vramAddr.reg);

            if (_vramAddr.reg >= 0x3F00) {
                // Palette read returns real data immediately
                data = buffer;
                // But buffer is updated with mirrored nametable byte
                PpuRead(_vramAddr.reg & 0x2FFF);
            }
            else {
                // Normal read uses buffered data
                data = buffer;
            }

            _vramAddr.reg += (_control.incrementMode ? 32 : 1);
            break;
        }
        default:
            data = _openBus;
            break;
        }
    }

    _openBus = data;
    return data;
}

void PPU2C02_Registers::CpuWrite(uint16_t addr, uint8_t data) {
    _openBus = data;

    switch (addr) {
    case 0x0000: // PPUCTRL ($2000)
        _control.reg = data;
        _tramAddr.nametableX = _control.nametableX;
        _tramAddr.nametableY = _control.nametableY;
        break;

    case 0x0001: // PPUMASK ($2001)
        _mask.reg = data;
        break;

    case 0x0002: // PPUSTATUS ($2002)
        // Read-only register, writes are ignored
        break;

    case 0x0005: // PPUSCROLL ($2005)
        if (_addressLatch == 0) {
            _fineX = data & 0x07;
            _tramAddr.coarseX = data >> 3;
            _addressLatch = 1;
        }
        else {
            _tramAddr.fineY = data & 0x07;
            _tramAddr.coarseY = data >> 3;
            _addressLatch = 0;
        }
        break;

    case 0x0006: // PPUADDR ($2006)
        if (_addressLatch == 0) {
            _tramAddr.reg = (uint16_t)((data & 0x3F) << 8) | (_tramAddr.reg & 0x00FF);
            _addressLatch = 1;
        }
        else {
            _tramAddr.reg = (_tramAddr.reg & 0xFF00) | data;
            _vramAddr = _tramAddr;
            _addressLatch = 0;
        }
        break;

    case 0x0007: // PPUDATA ($2007)
        PpuWrite(_vramAddr.reg, data);
        _vramAddr.reg += (_control.incrementMode ? 32 : 1);
        break;
    }
}