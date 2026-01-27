#include "../core/Cartridge.h"

bool Cartridge::IsLoaded() const { 
    return _isLoaded; 
}

uint8_t Cartridge::MapperID() const { 
    return _header.get_mapper_number(); 
}

uint8_t Cartridge::PrgBanks() const { 
    return _header.prg_rom_size; 
}

uint8_t Cartridge::ChrBanks() const { 
    return (_header.chr_rom_size == 0 ? 1 : _header.chr_rom_size); 
}

MirrorMode Cartridge::GetMirrorMode() const {
    if (_mapper != nullptr) {
        MirrorMode mapperMirror = _mapper->GetMirrorMode();
        if (mapperMirror != MirrorMode::Hardware) {
            return mapperMirror;
        }
    }

    if (_header.is_four_screen_mode()) {
        return MirrorMode::FourScreen;
    }
    else if (_header.is_vertical_mirroring()) {
        return MirrorMode::Vertical;
    }
    else {
        return MirrorMode::Horizontal;
    }
}

bool Cartridge::HasBattery() const { 
    return _header.has_battery_backed_ram(); 
}

MapperBase* Cartridge::GetMapper() const {
    if (_mapper != nullptr) {
        return _mapper.get();
    }
    return nullptr;
}

void Cartridge::EnableLogging(bool enable) {
    _loggingEnabled = enable;
}
