#include "pch.h"
#include "../core/Cartridge.h"

bool Cartridge::Load(const char* path) {
	return true;
}

bool Cartridge::CpuRead(uint16_t addr, uint8_t& data) {
	return true;
}

bool Cartridge::CpuWrite(uint16_t addr, uint8_t data)
{
	return true;
}

#pragma region "Exported Cartridge Functions"

DLLEXPORT Cartridge* CreateCartridge() { 
	return new Cartridge(); 
}

DLLEXPORT void DestroyCartridge(Cartridge* cart) { 
	delete cart; 
}

typedef void(__stdcall* DiagnosticCallback)(const char* message);
DLLEXPORT bool LoadRom(Cartridge* cart, const char* path, DiagnosticCallback callback) {
	bool result = cart->Load(path);
	if (result && callback) callback("ROM loaded successfully.");
	else if (callback) callback("Error: ROM load failed.");
	return result;
}

DLLEXPORT bool CartCpuRead(Cartridge* cart, uint16_t addr, uint8_t* data) {
	return cart->CpuRead(addr, *data);
}

#pragma endregion