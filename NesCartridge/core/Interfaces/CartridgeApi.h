#pragma once

/*
	Your EXPORT/IMPORT should be something like -J
	#if defined(_WIN32)
		#define EXPORT __declspec(dllexport)
		#define IMPORT __declspec(dllimport)
	#endif
	#if defined(__linux)
		#define EXPORT __attribute__((visibility("default")))
		#define IMPORT
	#endif

	Your API extraction header will look like so your welcome
	-J

	#define PLATOFORM_HEADERS_REQUIRED

	#include "CartridgeInterfaceAPI.h"

	DLLHANDLE LoadDll(const char* path) {
	#if defined(_WIN32)
		return LoadLibraryA(path);
	#else
		// you may need to fix this.
		return dlopen(path, RTLD_LAZY);
	#endif
	}
	void CloseDll(DLLHANDLE loaded_dll) {
	#if defined(_WIN32)
		FreeLibrary(loaded_dll);
	#else
		dlclose(loaded_dll);
	#endif
	}
	void* GetSignature(DLLHANDLE loaded_dll, const char* name) {
	#if defined(_WIN32)
		return GetProcAddress(loaded_dll, name);
	#else
		return dlsym(loaded_dll, name);
	#endif
	}

	MapperInterfaceAPI::MapperInterfaceAPI()
	{
		_dll = LoadDll(DLL_CARTRIDGE_NAME);
		if (_dll) {
			LPFN_GET_MAPPER_API m_pGetCartApi = reinterpret_cast<LPFN_GET_MAPPER_API>(GetSignature(_dll, LPFN_GET_MAPPER_API_NAME));
			if (m_pGetCartApi != nullptr) {
				m_pGetCartApi(&_api);
			}
			else {
				CloseDll(_dll); _dll = nullptr;
				throw("Could not find function Signature.");
			}
		}
		else {
			throw("Shared Object failed to load.");
		}
	}

	MapperInterfaceAPI::~MapperInterfaceAPI()
	{
		CloseDll(_dll);
	}

	CartridgeInterfaceAPI::CartridgeInterfaceAPI()
	{
		_dll = LoadDll(DLL_CARTRIDGE_NAME);
		if (_dll) {
			LPFN_GET_CARTRIDGE_API m_pGetCartApi = reinterpret_cast<LPFN_GET_CARTRIDGE_API>(GetSignature(_dll, LPFN_GET_CARTRIDGE_API_NAME));
			if (m_pGetCartApi != nullptr) {
				m_pGetCartApi(&_api);
			}
			else {
				CloseDll(_dll); _dll = nullptr;
				throw("Could not find function Signature.");
			}
		}
		else {
			throw("Shared Object failed to load.");
		}
	}

	CartridgeInterfaceAPI::~CartridgeInterfaceAPI()
	{
		if (_dll)
			CloseDll(_dll);
	}
*/

#include <cstdint>
#include "MirrorModeRequired.h"

#if defined(PLATOFORM_HEADERS_REQUIRED)
	#if defined(_WIN32)
		#include <Windows.h> // LoadLibrary, FreeLibrary
		using DLLHANDLE = HMODULE;
	#else
		#include <dlfcn.h>   // dlopen, dlclose (you can find the rest of the headers required for linux, etc.)
		using DLLHANDLE = void*;
	#endif
#endif

typedef void(__stdcall* DiagnosticLogCallback)(const char* message);

// Forward declarations
using MIRRORMODE = MirrorMode;

using CARTRIDGE = class Cartridge;
using LPCARTRIDGE = CARTRIDGE*;
using MAPPERBASE = class MapperBase;
using LPMAPPERBASE = MAPPERBASE*;

#if defined(_WIN32)
	constexpr const char* DLL_CARTRIDGE_NAME = "NesCartridge.dll";
#else
	/* 
		its highly unlikely that i will be building a linux dll set
		however it dosent mean i wont try to make it easyier for
		whoever desides to try to do so.
	*/
	constexpr const char* DLL_CARTRIDGE_NAME = "NesCartridge.so";
#endif

#pragma region "Cartridge Exported Names and Signatures"
constexpr const char* LPFN_CREATE_CARTRIDGE_NAME = "CreateCartridge";
typedef LPCARTRIDGE(*LPFN_CREATE_CARTRIDGE)();

constexpr const char* LPFN_CREATE_CARTRIDGE_ATTACH_DIAGNOSTICS_NAME = "CreateCartridgeDiag";
typedef LPCARTRIDGE(*LPFN_CREATE_CARTRIDGE_ATTACH_DIAGNOSTICS)(DiagnosticLogCallback);

constexpr const char* LPFN_CARTRIDGE_SET_DIAGNOSTICS_LOG_CALLBACK_NAME = "CartridgeSetDiagnosticLogCallback";
typedef void(*LPFN_CARTRIDGE_SET_DIAGNOSTICS_LOG_CALLBACK)(LPCARTRIDGE, DiagnosticLogCallback);

constexpr const char* LPFN_DESTROY_CARTRIDGE_NAME = "DestroyCartridge";
typedef void(*LPFN_DESTROY_CARTRIDGE)(LPCARTRIDGE);

constexpr const char* LPFN_LOAD_CARTRIDGE_NAME = "LoadCartridge";
typedef bool(*LPFN_LOAD_CARTRIDGE)(LPCARTRIDGE, const char*);

constexpr const char* LPFN_CARTRIDGE_ENABLE_LOGGING_NAME = "CartridgeEnableLogging";
typedef void(*LPFN_CARTRIDGE_ENABLE_LOGGING)(LPCARTRIDGE, bool);

constexpr const char* LPFN_CARTRIDGE_GET_MIRRORMODE_NAME = "CartridgeGetMirrorMode";
typedef MIRRORMODE(*LPFN_CARTRIDGE_GET_MIRRORMODE)(LPCARTRIDGE);

constexpr const char* LPFN_CARTRIDGE_IS_LOADED_NAME = "CartridgeIsLoaded";
typedef bool(*LPFN_CARTRIDGE_IS_LOADED)(LPCARTRIDGE);

//constexpr const char* LPFN_CARTRIDGE_CLOCK_NAME = "CartridgeClock";
//typedef void(*LPFN_CARTRIDGE_CLOCK)(LPCARTRIDGE);

constexpr const char* LPFN_CARTRIDGE_CPU_READ_NAME = "CartCpuRead";
typedef bool(*LPFN_CARTRIDGE_CPU_READ)(LPCARTRIDGE, uint16_t, uint8_t*);

constexpr const char* LPFN_CARTRIDGE_CPU_WRITE_NAME = "CartCpuWrite";
typedef bool(*LPFN_CARTRIDGE_CPU_WRITE)(LPCARTRIDGE, uint16_t, uint8_t);

constexpr const char* LPFN_CARTRIDGE_PPU_READ_NAME = "CartPpuRead";
typedef bool(*LPFN_CARTRIDGE_PPU_READ)(LPCARTRIDGE, uint16_t, uint8_t*);

constexpr const char* LPFN_CARTRIDGE_PPU_WRITE_NAME = "CartPpuWrite";
typedef bool(*LPFN_CARTRIDGE_PPU_WRITE)(LPCARTRIDGE, uint16_t, uint8_t);

constexpr const char* LPFN_RESET_CARTRIDGE_NAME = "ResetCartridge";
typedef void(*LPFN_RESET_CARTRIDGE)(LPCARTRIDGE);

constexpr const char* LPFN_CARTRIDGE_MAPPER_NAME = "CartridgeMapper";
typedef LPMAPPERBASE(*LPFN_CARTRIDGE_MAPPER)(LPCARTRIDGE);

struct CartridgeApi {
	LPFN_CREATE_CARTRIDGE CreateCartridge;
	LPFN_CREATE_CARTRIDGE_ATTACH_DIAGNOSTICS CreateCartridgeDiag;
	LPFN_CARTRIDGE_SET_DIAGNOSTICS_LOG_CALLBACK CartridgeSetDiagnosticLogCallback;
	LPFN_DESTROY_CARTRIDGE DestroyCartridge;
	LPFN_LOAD_CARTRIDGE LoadCartridge;
	LPFN_CARTRIDGE_ENABLE_LOGGING CartridgeEnableLogging;
	LPFN_CARTRIDGE_GET_MIRRORMODE CartridgeGetMirrorMode;
	LPFN_CARTRIDGE_IS_LOADED CartridgeIsLoaded;
	//LPFN_CARTRIDGE_CLOCK CartridgeClock;
	LPFN_CARTRIDGE_CPU_READ CartCpuRead;
	LPFN_CARTRIDGE_CPU_WRITE CartCpuWrite;
	LPFN_CARTRIDGE_PPU_READ CartPpuRead;
	LPFN_CARTRIDGE_PPU_WRITE CartPpuWrite;
	LPFN_RESET_CARTRIDGE ResetCartridge;
	LPFN_CARTRIDGE_MAPPER CartridgeMapper;
};
using LPCARTRIDGEAPI = CartridgeApi*;
#pragma endregion

#pragma region "Cartridge Mapper Exported Names and Signatures"
constexpr const char* LPFN_MAPPER_IS_IRQ_ACTIVE_NAME = "MapperIsIrqActive";
typedef bool(*LPFN_MAPPER_IS_IRQ_ACTIVE)(LPMAPPERBASE);

constexpr const char* LPFN_MAPPER_CLEAR_IRQ_NAME = "MapperClearIrq";
typedef void(*LPFN_MAPPER_CLEAR_IRQ)(LPMAPPERBASE);

constexpr const char* LPFN_MAPPER_RESET_NAME = "MapperReset";
typedef void(*LPFN_MAPPER_RESET)(LPMAPPERBASE);

constexpr const char* LPFN_MAPPER_GET_MIRRORMODE_NAME = "MapperGetMirrorMode";
typedef MIRRORMODE(*LPFN_MAPPER_GET_MIRRORMODE)(LPMAPPERBASE);

constexpr const char* LPFN_MAPPER_SCANLINE_COUNTER_NAME = "MapperScanlineCounter";
typedef void(*LPFN_MAPPER_SCANLINE_COUNTER)(LPMAPPERBASE);

struct MapperApi {
	LPFN_MAPPER_IS_IRQ_ACTIVE MapperIsIrqActive;
	LPFN_MAPPER_CLEAR_IRQ MapperClearIrq;
	LPFN_MAPPER_RESET MapperReset;
	LPFN_MAPPER_GET_MIRRORMODE MapperGetMirrorMode;
	LPFN_MAPPER_SCANLINE_COUNTER MapperScanlineCounter;
};
using LPMAPPERAPI = MapperApi*;
#pragma endregion

#pragma region "Cartridge Main API Export"

constexpr const char* LPFN_GET_CARTRIDGE_API_NAME = "GetCartridgeAPI";
typedef void(*LPFN_GET_CARTRIDGE_API)(LPCARTRIDGEAPI);
constexpr const char* LPFN_GET_MAPPER_API_NAME = "GetMapperAPI";
typedef void(*LPFN_GET_MAPPER_API)(LPMAPPERAPI);

#pragma endregion