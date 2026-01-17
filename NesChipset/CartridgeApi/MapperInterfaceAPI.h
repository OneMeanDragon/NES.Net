#pragma once

#include <core/Interfaces/CartridgeApi.h>

class MapperInterfaceAPI {
private:
	LPMAPPERAPI _api = nullptr;
	LPMAPPERBASE _mapper = nullptr;
private:
	MapperInterfaceAPI() = delete;
	MapperInterfaceAPI(LPMAPPERAPI api);
public:
	MapperInterfaceAPI(LPMAPPERAPI api, LPMAPPERBASE _mapper);
	virtual ~MapperInterfaceAPI() {};
public:
	bool IsIrqActive();
	void ClearIrq();
	void Reset();
	MirrorMode GetMirrorMode();
	void ScanlineCounter();
};
