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
    switch (addr & 0xFu) {
    case 0x0000u: return _openBus;// PPUCTRL   ($2000) - Write-only
    case 0x0001u: return _openBus;// PPUMASK   ($2001) - Write-only
    case 0x0003u: return _openBus;// OAMADDR   ($2003) - Write-only
    case 0x0005u: return _openBus;// PPUSCROLL ($2005) - Write-only
    case 0x0006u: return _openBus;// PPUADDR   ($2006) - Write-only

    case 0x0002u: {// PPUSTATUS ($2002) - Read-only
        _openBus = (_status.reg & 0xE0u) | (_openBus & 0x1Fu);

        // --- NMI Suppression Logic ---
        // If we read $2002 right when VBlank starts, suppress the NMI
        //const int16_t scanline = GetScanline();
        //const int16_t cycle = GetCycle();
        //if (scanline == 241 && (cycle >= 0 && cycle <= 3)) { // Expanded window
        //    ClearNmiRequested();
        //    if (cycle == 0) _openBus &= 0x7F;
        //}

        _status.verticalBlank = 0; // clear VBL flag on read
        _addressLatch = 0; // reset latch
        return _openBus;
    }
    case 0x0004u: {// OAMDATA ($2004) - Read/Write
        _openBus = ReadOAM();
        return _openBus;
    }
    case 0x0007u: {// PPUDATA ($2007) - Read/Write
        uint16_t vAddr = _vramAddr.reg;
        uint8_t value = _dataBuffer;

        // Read from PPU memory
        _dataBuffer = PpuRead(vAddr);

        // Palette memory reads bypass the buffer
        if (vAddr >= 0x3F00u) {
            value = _dataBuffer;
        }

        // Increment VRAM address
        _vramAddr.reg = (_vramAddr.reg + (_control.incrementMode ? 32 : 1));
        _openBus = value;
        return value;
    }

    default: return _openBus;
    }

    return 0x00;
}

//bool write_mode = false; write mode is the address latch
void PPU2C02_Registers::CpuWrite(uint16_t addr, uint8_t data) {
    _openBus = data & 0xFFu;

    switch (addr & 0xfu) {
    case 0x0000u: {// PPUCTRL ($2000)
        bool oldNmiEnable = _control.enableNmi;

        _control.reg = data;
        _tramAddr.SetNametableX(data & 0x1u);
        _tramAddr.SetNametableY((data >> 1) & 0x1u);

        // --------------------------------------------------
        // NMI edge cancellation (REQUIRED for Ms. Pac-Man)
        // --------------------------------------------------
        if (oldNmiEnable && !_control.enableNmi && _status.verticalBlank)
        {
            ClearNmiRequested();
        }
        break;
    }

    case 0x0001u: // PPUMASK ($2001)
        _mask.reg = data;
        break;

    case 0x0002u: return;// PPUSTATUS ($2002) // Read-only register, writes are ignored 

    case 0x0003u: // OAMADDR ($2003)
        SetOAMAddress(data);
        break;

    case 0x0004u: // OAMDATA ($2004)
        WriteOAM(data);
        break;

    case 0x0005u: // PPUSCROLL ($2005)
        if (_addressLatch == 0) {
            _fineX = data & 0x07u;
            _tramAddr.SetCoarseX(data >> 3);
            _addressLatch = 1;
        }
        else {
            _tramAddr.SetFineY(data & 0x07u);
            _tramAddr.SetCoarseY(data >> 3);
            _addressLatch = 0;
        }
        break;

    case 0x0006u: // PPUADDR ($2006)
        if (_addressLatch == 0) {
            _tramAddr.reg = (_tramAddr.reg & 0x00FFu) | ((data & 0x3Fu) << 8);
            _addressLatch = 1;
        }
        else {
            _tramAddr.reg = (_tramAddr.reg & 0xFF00u) | data;
            // v = t happens ONLY on the second write to $2006
            _vramAddr.reg = _tramAddr.reg;
            _addressLatch = 0;
        }
        break;

    case 0x0007u: // PPUDATA ($2007)
        PpuWrite(_vramAddr.reg, data);
        _vramAddr.reg += (_control.incrementMode ? 32 : 1);
        break;
    }
}