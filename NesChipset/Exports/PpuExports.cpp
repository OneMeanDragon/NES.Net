#include "../PPU2C02/PPU2C02.h"

#ifdef _WIN32
    #define DLLEXPORT extern "C" __declspec(dllexport)
#else
    #define DLLEXPORT
#endif

// Exported PPU functions
DLLEXPORT PPU2C02* CreatePPU() {
    return new PPU2C02();
}

DLLEXPORT void DestroyPPU(PPU2C02* ppu) {
    delete ppu;
}

DLLEXPORT void PPU_Reset(PPU2C02* ppu, bool coldstart) {
    if (ppu) ppu->Reset(coldstart);
}

DLLEXPORT void PPU_Clock(PPU2C02* ppu) {
    if (ppu) ppu->Clock();
}

DLLEXPORT uint8_t PPU_CpuRead(PPU2C02* ppu, uint16_t addr, bool rdOnly) {
    if (ppu) return ppu->CpuRead(addr, rdOnly);
    return 0;
}

DLLEXPORT void PPU_CpuWrite(PPU2C02* ppu, uint16_t addr, uint8_t data) {
    if (ppu) ppu->CpuWrite(addr, data);
}

// PPU Bus Interface
DLLEXPORT uint8_t PPU_PpuRead(PPU2C02* ppu, uint16_t addr, bool rdOnly) {
    if (ppu) return ppu->PpuRead(addr, rdOnly);
    return 0;
}

DLLEXPORT void PPU_PpuWrite(PPU2C02* ppu, uint16_t addr, uint8_t data) {
    if (ppu) ppu->PpuWrite(addr, data);
}

DLLEXPORT bool PPU_IsFrameComplete(PPU2C02* ppu) {
    if (ppu) return ppu->IsFrameComplete();
    return false;
}

DLLEXPORT void PPU_SetFrameComplete(PPU2C02* ppu, bool value) {
    if (ppu) ppu->SetFrameComplete(value);
}

DLLEXPORT bool PPU_GetNmiRequested(PPU2C02* ppu) {
    if (ppu) return ppu->GetNmiRequested();
    return false;
}

DLLEXPORT void PPU_ClearNmiRequested(PPU2C02* ppu) {
    if (ppu) ppu->ClearNmiRequested();
}

DLLEXPORT void PPU_SetPixelCallback(PPU2C02* ppu, PixelCallback callback) {
    if (ppu) ppu->SetPixelCallback(callback);
}

DLLEXPORT void PPU_SetDiagnosticCallback(PPU2C02* ppu, DiagnosticLogCallback callback) {
    if (ppu) ppu->SetDiagnosticCallback(callback);
}

DLLEXPORT void PPU_GetPatternTable(PPU2C02* ppu, uint8_t table, uint8_t palette, uint8_t* buffer) {
    if (ppu) ppu->GetPatternTable(table, palette, buffer);
}

DLLEXPORT void PPU_GetOAMEntry(PPU2C02* ppu, uint8_t index, OAMEntry* entry) {
    if (ppu && entry && index < 64) {
        // Access through public method
        const OAMEntry* oam = ppu->GetOAM();
        *entry = oam[index];
    }
}

DLLEXPORT void PPU_SetOAMEntry(PPU2C02* ppu, uint8_t index, OAMEntry* entry) {
    if (ppu && entry && index < 64) {
        // Access through mutable method
        OAMEntry* oam = ppu->GetOAMMutable();
        oam[index] = *entry;
    }
}

DLLEXPORT uint8_t PPU_GetOAMByte(PPU2C02* ppu, uint8_t oamAddr) {
    if (!ppu) return 0xFF;

    // Access through public method and cast to byte array
    const OAMEntry* oam = ppu->GetOAM();
    return reinterpret_cast<const uint8_t*>(oam)[oamAddr];
}

DLLEXPORT void PPU_SetOAMByte(PPU2C02* ppu, uint8_t oamAddr, uint8_t data) {
    if (!ppu) return;

    // Access through mutable method and cast to byte array
    OAMEntry* oam = ppu->GetOAMMutable();
    reinterpret_cast<uint8_t*>(oam)[oamAddr] = data;
}

DLLEXPORT void PPU_GetColorFromPalette(PPU2C02* ppu, uint8_t palette, uint8_t pixel, uint8_t* r, uint8_t* g, uint8_t* b) {
    if (ppu && r && g && b) {
        Pixel color = ppu->GetColorFromPalette(palette, pixel);
        *r = color.r;
        *g = color.g;
        *b = color.b;
    }
}