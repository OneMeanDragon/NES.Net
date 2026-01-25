#define PLATOFORM_HEADERS_REQUIRED
#include "CartridgeInterfaceAPI.h"
#include "MapperInterfaceAPI.h"
#include <stdexcept>

/*
	yes we are set to explode if the functions arent found.
*/

CartridgeInterfaceAPI::CartridgeInterfaceAPI()
{
	try {

		_dll = LoadDll(DLL_CARTRIDGE_NAME);
		if (_dll) {
			LPFN_GET_CARTRIDGE_API m_pGetCartApi = reinterpret_cast<LPFN_GET_CARTRIDGE_API>(GetSignature(_dll, LPFN_GET_CARTRIDGE_API_NAME));
			if (m_pGetCartApi != nullptr) {
				m_pGetCartApi(&_api);
				if (_api.CartCpuRead == nullptr || _api.CartCpuWrite == nullptr || _api.CartPpuRead == nullptr || _api.CartPpuWrite == nullptr || /*_api.CartridgeClock == nullptr ||*/ _api.CartridgeEnableLogging == nullptr || _api.CartridgeGetMirrorMode == nullptr || _api.CartridgeIsLoaded == nullptr || _api.CartridgeMapper == nullptr || _api.CartridgeSetDiagnosticLogCallback == nullptr || _api.CreateCartridge == nullptr || _api.CreateCartridgeDiag == nullptr || _api.DestroyCartridge == nullptr || _api.LoadCartridge == nullptr || _api.ResetCartridge == nullptr) {
					CloseDll(_dll); _dll = nullptr;
					if (_api.CartCpuRead == nullptr)                       throw std::runtime_error("Could not find function Signature CartCpuRead.");
					if (_api.CartCpuWrite == nullptr)                      throw std::runtime_error("Could not find function Signature CartCpuWrite.");
					if (_api.CartPpuRead == nullptr)                       throw std::runtime_error("Could not find function Signature CartPpuRead.");
					if (_api.CartPpuWrite == nullptr)                      throw std::runtime_error("Could not find function Signature CartPpuWrite.");
					//if (_api.CartridgeClock == nullptr)                    throw std::runtime_error("Could not find function Signature CartridgeClock.");
					if (_api.CartridgeEnableLogging == nullptr)            throw std::runtime_error("Could not find function Signature CartridgeEnableLogging.");
					if (_api.CartridgeGetMirrorMode == nullptr)            throw std::runtime_error("Could not find function Signature CartridgeGetMirrorMode.");
					if (_api.CartridgeIsLoaded == nullptr)                 throw std::runtime_error("Could not find function Signature CartridgeIsLoaded.");
					if (_api.CartridgeMapper == nullptr)                   throw std::runtime_error("Could not find function Signature CartridgeMapper.");
					if (_api.CartridgeSetDiagnosticLogCallback == nullptr) throw std::runtime_error("Could not find function Signature CartridgeSetDiagnosticLogCallback.");
					if (_api.CreateCartridge == nullptr)                   throw std::runtime_error("Could not find function Signature CreateCartridge.");
					if (_api.CreateCartridgeDiag == nullptr)               throw std::runtime_error("Could not find function Signature CreateCartridgeDiag.");
					if (_api.DestroyCartridge == nullptr)                  throw std::runtime_error("Could not find function Signature DestroyCartridge.");
					if (_api.LoadCartridge == nullptr)                     throw std::runtime_error("Could not find function Signature LoadCartridge.");
					if (_api.ResetCartridge == nullptr)                    throw std::runtime_error("Could not find function Signature ResetCartridge.");
				}
			}
			else {
				CloseDll(_dll); _dll = nullptr;
				throw std::runtime_error("Could not find function Signature CartridgeAPI.");
			}
			LPFN_GET_MAPPER_API m_pGetMapperApi = reinterpret_cast<LPFN_GET_MAPPER_API>(GetSignature(_dll, LPFN_GET_MAPPER_API_NAME));
			if (m_pGetMapperApi != nullptr) {
				m_pGetMapperApi(&_apimapper);
				if (_apimapper.MapperClearIrq == nullptr || _apimapper.MapperGetMirrorMode == nullptr || _apimapper.MapperIsIrqActive == nullptr || _apimapper.MapperReset == nullptr || _apimapper.MapperScanlineCounter == nullptr) {
					CloseDll(_dll); _dll = nullptr;
					if (_apimapper.MapperClearIrq == nullptr)        throw std::runtime_error("Could not find function Signature MapperClearIrq.");
					if (_apimapper.MapperGetMirrorMode == nullptr)   throw std::runtime_error("Could not find function Signature MapperGetMirrorMode.");
					if (_apimapper.MapperIsIrqActive == nullptr)     throw std::runtime_error("Could not find function Signature MapperIsIrqActive.");
					if (_apimapper.MapperReset == nullptr)           throw std::runtime_error("Could not find function Signature MapperReset.");
					if (_apimapper.MapperScanlineCounter == nullptr) throw std::runtime_error("Could not find function Signature MapperScanlineCounter.");
				}
			}
			else {
				CloseDll(_dll); _dll = nullptr;
				throw std::runtime_error("Could not find function Signature GetMapperAPI.");
			}
		}
		else {
			throw std::runtime_error("Shared Object failed to load.");
		}

	}
	catch (const std::exception& e) {
		// Convert the exception message to a wide character string (for Unicode support)
		// Note: this simple conversion is suitable for basic ASCII strings
		std::string narrow_message = e.what();
		std::wstring wide_message(narrow_message.begin(), narrow_message.end());

		// Display the error message in a Windows message box
		MessageBox(
			NULL,                               // Owner window handle (NULL for no owner)
			wide_message.c_str(),               // Message to display
			L"Error (Application Crash)",                           // Dialog box title
			MB_ICONERROR | MB_OK                // Icon and buttons (Error icon and OK button)
		);
	}
	catch (...) {
		// Handle any other exception types
		MessageBox(NULL, L"An unknown error occurred.", L"Error (Application Crash)", MB_ICONERROR | MB_OK);
	}
}

CartridgeInterfaceAPI::CartridgeInterfaceAPI(LPCARTRIDGE cart)
	: CartridgeInterfaceAPI()
{
	_cartridge = cart;
}

CartridgeInterfaceAPI::CartridgeInterfaceAPI(LPCARTRIDGE cart, DiagnosticLogCallback callback)
	: CartridgeInterfaceAPI(cart)
{
	_callback = callback;
}

CartridgeInterfaceAPI::~CartridgeInterfaceAPI()
{
	DestroyCartridge();
	if (_dll)
		CloseDll(_dll);
}

LPCARTRIDGE CartridgeInterfaceAPI::CreateCartridge()
{
	return _api.CreateCartridge();
}

LPCARTRIDGE CartridgeInterfaceAPI::CreateCartridgeDiag(DiagnosticLogCallback callback)
{
	return _api.CreateCartridgeDiag(callback);
}

void CartridgeInterfaceAPI::DestroyCartridge()
{
	//_api.DestroyCartridge(_cartridge); // curently we do not want to destroy the cartridge here
	_cartridge = nullptr;                // we are destroying it from our wrapper
}

void CartridgeInterfaceAPI::SetDiagnosticLogCallback(DiagnosticLogCallback callback)
{
	_api.CartridgeSetDiagnosticLogCallback(_cartridge, callback);
}

bool CartridgeInterfaceAPI::Load(const char* path)
{
	return _api.LoadCartridge(_cartridge, path);
}

void CartridgeInterfaceAPI::EnableLogging(bool enable)
{
	_api.CartridgeEnableLogging(_cartridge, enable);
}

MirrorMode CartridgeInterfaceAPI::GetMirrorMode()
{
	return _api.CartridgeGetMirrorMode(_cartridge);
}

bool CartridgeInterfaceAPI::IsLoaded() const
{
	return _api.CartridgeIsLoaded(_cartridge);
}

//void CartridgeInterfaceAPI::Clock()
//{
//	_api.CartridgeClock(_cartridge);
//}

bool CartridgeInterfaceAPI::CpuRead(uint16_t addr, uint8_t* data)
{
	return _api.CartCpuRead(_cartridge, addr, data);
}

bool CartridgeInterfaceAPI::CpuWrite(uint16_t addr, uint8_t data)
{
	return _api.CartCpuWrite(_cartridge, addr, data);
}

bool CartridgeInterfaceAPI::PpuRead(uint16_t addr, uint8_t* data)
{
	return _api.CartPpuRead(_cartridge, addr, data);
}

bool CartridgeInterfaceAPI::PpuWrite(uint16_t addr, uint8_t data)
{
	return _api.CartPpuWrite(_cartridge, addr, data);
}

void CartridgeInterfaceAPI::Reset()
{
	_api.ResetCartridge(_cartridge);
}

MapperInterfaceAPI CartridgeInterfaceAPI::GetMapper()
{
	return MapperInterfaceAPI(&_apimapper, _api.CartridgeMapper(_cartridge));
}
