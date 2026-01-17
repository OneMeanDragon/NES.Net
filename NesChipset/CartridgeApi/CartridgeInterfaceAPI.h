#pragma once

#include <core/Interfaces/CartridgeApi.h>
#include "SharedObjectHelper.h"

class MapperInterfaceAPI;

class CartridgeInterfaceAPI {
private:
	DLLHANDLE _dll = nullptr;
	CartridgeApi _api{ 0 };
	MapperApi _apimapper{ 0 };
	LPCARTRIDGE _cartridge = nullptr;
	DiagnosticLogCallback _callback = nullptr;
private:
	CartridgeInterfaceAPI();
public:
	CartridgeInterfaceAPI(LPCARTRIDGE cart);
	CartridgeInterfaceAPI(LPCARTRIDGE cart, DiagnosticLogCallback callback);
	virtual ~CartridgeInterfaceAPI();

	LPCARTRIDGE CreateCartridge();
	LPCARTRIDGE CreateCartridgeDiag(DiagnosticLogCallback callback);
	void DestroyCartridge();

	void SetDiagnosticLogCallback(DiagnosticLogCallback callback);
	bool Load(const char* path);

	void EnableLogging(bool enable);
	MirrorMode GetMirrorMode();

	bool IsLoaded() const;
	void Clock();

	bool CpuRead(uint16_t addr, uint8_t* data);
	bool CpuWrite(uint16_t addr, uint8_t data);
	bool PpuRead(uint16_t addr, uint8_t* data);
	bool PpuWrite(uint16_t addr, uint8_t data);

	void Reset();

	MapperInterfaceAPI GetMapper();
};
