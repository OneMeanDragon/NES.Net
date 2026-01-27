#include "../core/Cartridge.h"

#ifdef _WIN32
#define DLLEXPORT extern "C" __declspec(dllexport)
#else
#define DLLEXPORT
#endif

#pragma region "Exported Cartridge Functions"
DLLEXPORT void CartridgeSetDiagnosticLogCallback(Cartridge* cart, DiagnosticLogCallback callback) {
    if (cart && callback) {
        cart->SetDiagnosticLogCallback(callback);
        cart->Log("Info: Diagnostic Log Callback attached successfully.");
    }
    else if (cart == nullptr) {
        if (callback) callback("Error: Cartridge instance is nullptr.");
    }
}

DLLEXPORT Cartridge* CreateCartridge() {
    return new Cartridge();
}

DLLEXPORT Cartridge* CreateCartridgeDiag(DiagnosticLogCallback callback) {
    Cartridge* cart = new Cartridge();
    if (cart == nullptr) {
        if (callback) callback("Error: Unable to create Cartridge instance.");
        return nullptr;
    }
    cart->SetDiagnosticLogCallback(callback);
    cart->Log("Native Cartridge instance created.");
    return cart;
}

DLLEXPORT void DestroyCartridge(Cartridge* cart) { // to be considered [Cartridge** cart]
    if (cart) {
        DiagnosticLogCallback callback = cart->_diagnosticCallback;
        bool loggingEnabled = cart->LoggingEnabled();
        delete cart;
        if (loggingEnabled && callback) {
            callback("Info: Cartridge destroyed.");
        }
    }
}

DLLEXPORT bool LoadCartridge(Cartridge* cart, const char* path) {
    if (cart && path) {
        bool result = cart->Load(path);
        if (result) cart->Log("Info: ROM loaded successfully.");
        else cart->Log("Error: ROM load failed.");
        return result;
    }
    return false;
}

DLLEXPORT void CartridgeEnableLogging(Cartridge* cart, bool enable) {
    if (cart) return cart->EnableLogging(enable);
}

DLLEXPORT MirrorMode CartridgeGetMirrorMode(Cartridge* cart) {
    if (cart) return cart->GetMirrorMode();
    return MirrorMode::Hardware;
}

DLLEXPORT bool CartridgeIsLoaded(Cartridge* cart) {
    if (cart) return cart->IsLoaded();
    return false;
}

DLLEXPORT bool CartCpuRead(Cartridge* cart, uint16_t addr, uint8_t* data) {
    if (cart) return cart->CpuRead(addr, *data);
    return false;
}

DLLEXPORT bool CartCpuWrite(Cartridge* cart, uint16_t addr, uint8_t data) {
    if (cart) return cart->CpuWrite(addr, data);
    return false;
}

DLLEXPORT bool CartPpuRead(Cartridge* cart, uint16_t addr, uint8_t* data) {
    if (cart) return cart->PpuRead(addr, *data);
    return false;
}

DLLEXPORT bool CartPpuWrite(Cartridge* cart, uint16_t addr, uint8_t data) {
    if (cart) return cart->PpuWrite(addr, data);
    return false;
}

#pragma region "Exported MapperBase Functions"
// ============================= MAPPER RELATED Functionality =============================
DLLEXPORT MapperBase* CartridgeMapper(Cartridge* cart) {
    if (cart) return cart->GetMapper();
    return nullptr;
}
DLLEXPORT bool MapperIsIrqActive(MapperBase* mapper) {
    if (mapper) return mapper->IsIrqActive();
    return false;
}
DLLEXPORT void MapperClearIrq(MapperBase* mapper) {
    if (mapper) mapper->ClearIrq();
}
DLLEXPORT void MapperReset(MapperBase* mapper) {
    if (mapper) mapper->Reset();
}
DLLEXPORT MirrorMode MapperGetMirrorMode(MapperBase* mapper) {
    if (mapper) return mapper->GetMirrorMode();
    return MirrorMode::Hardware;
}
DLLEXPORT void MapperScanlineCounter(MapperBase* mapper, int16_t scanline) {
    if (mapper) mapper->ScanlineCounter(scanline);
}
#pragma endregion

DLLEXPORT void ResetCartridge(Cartridge* cart) {
    if (cart) {
        // Cartridge doesn't have Reset, but mapper does
        MapperBase* mapper = cart->GetMapper();
        if (mapper) {
            mapper->Reset();
        }
    }
}

#pragma endregion

/* API Section for the NesChipset.dll */
#include "../core/Interfaces/CartridgeApi.h"
DLLEXPORT void GetCartridgeAPI(LPCARTRIDGEAPI api_cartridge) {
    // CARTRIDGE API
    api_cartridge->CreateCartridge = CreateCartridge;
    api_cartridge->CreateCartridgeDiag = CreateCartridgeDiag;
    api_cartridge->CartridgeSetDiagnosticLogCallback = CartridgeSetDiagnosticLogCallback;
    api_cartridge->DestroyCartridge = DestroyCartridge;
    api_cartridge->LoadCartridge = LoadCartridge;
    api_cartridge->CartridgeEnableLogging = CartridgeEnableLogging;
    api_cartridge->CartridgeGetMirrorMode = CartridgeGetMirrorMode;
    api_cartridge->CartridgeIsLoaded = CartridgeIsLoaded;
    //api_cartridge->CartridgeClock = CartridgeClock;
    api_cartridge->CartCpuRead = CartCpuRead;
    api_cartridge->CartCpuWrite = CartCpuWrite;
    api_cartridge->CartPpuRead = CartPpuRead;
    api_cartridge->CartPpuWrite = CartPpuWrite;
    api_cartridge->ResetCartridge = ResetCartridge;
    api_cartridge->CartridgeMapper = CartridgeMapper;
}

DLLEXPORT void GetMapperAPI(LPMAPPERAPI api_mapper) {
    // MAPPER API
    api_mapper->MapperIsIrqActive = MapperIsIrqActive;
    api_mapper->MapperClearIrq = MapperClearIrq;
    api_mapper->MapperReset = MapperReset;
    api_mapper->MapperGetMirrorMode = MapperGetMirrorMode;
    api_mapper->MapperScanlineCounter = MapperScanlineCounter;
}
