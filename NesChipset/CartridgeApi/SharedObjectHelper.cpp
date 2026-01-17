#define PLATOFORM_HEADERS_REQUIRED
#include "SharedObjectHelper.h"

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