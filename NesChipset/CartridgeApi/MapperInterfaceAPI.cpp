#include "MapperInterfaceAPI.h"


MapperInterfaceAPI::MapperInterfaceAPI(LPMAPPERAPI api)
	: _api(api)
{}

MapperInterfaceAPI::MapperInterfaceAPI(LPMAPPERAPI api, LPMAPPERBASE mapper)
	: MapperInterfaceAPI(api)
{
	_mapper = mapper;
}

bool MapperInterfaceAPI::IsIrqActive()
{
	return _api->MapperIsIrqActive(_mapper);
}

void MapperInterfaceAPI::ClearIrq()
{
	_api->MapperClearIrq(_mapper);
}

void MapperInterfaceAPI::Reset()
{
	_api->MapperReset(_mapper);
}

MirrorMode MapperInterfaceAPI::GetMirrorMode()
{
	return _api->MapperGetMirrorMode(_mapper);
}

void MapperInterfaceAPI::ScanlineCounter()
{
	_api->MapperScanlineCounter(_mapper);
}
