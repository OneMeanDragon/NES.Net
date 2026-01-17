#pragma once

#if defined(PLATOFORM_HEADERS_REQUIRED)
	#if defined(_WIN32)
		#include <Windows.h> // LoadLibrary, FreeLibrary
		using DLLHANDLE = HMODULE;
	#else
		#include <dlfcn.h>   // dlopen, dlclose
		using DLLHANDLE = void*;
	#endif
#else
	using DLLHANDLE = void*;
#endif

DLLHANDLE LoadDll(const char* path);
void CloseDll(DLLHANDLE loaded_dll);
void* GetSignature(DLLHANDLE loaded_dll, const char* name);