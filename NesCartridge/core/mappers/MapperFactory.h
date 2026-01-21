#pragma once
#include <memory>
#include <string>
#include <vector>
#include "MapperBase.h"
// Include all mapper headers
#include "Mapper000.h"
#include "Mapper001.h"
#include "Mapper002.h"
#include "Mapper003.h"
#include "Mapper004.h"
#include "Mapper009.h"
#include "Mapper066.h"
#include "Mapper206.h"

namespace nes {

    class MapperFactory {
    public:
        // Delete constructor to make it a static-only class
        MapperFactory() = delete;

        static std::unique_ptr<MapperBase> CreateMapper(uint8_t mapperNumber, uint8_t prgBanks, uint8_t chrBanks) {
            switch (mapperNumber) {
            case 0:  return std::make_unique<Mapper000>(prgBanks, chrBanks);
            case 1:  return std::make_unique<Mapper001>(prgBanks, chrBanks);
            case 2:  return std::make_unique<Mapper002>(prgBanks, chrBanks);
            case 3:  return std::make_unique<Mapper003>(prgBanks, chrBanks);
            case 4:  return std::make_unique<Mapper004>(prgBanks, chrBanks);
            case 9:  return std::make_unique<Mapper009>(prgBanks, chrBanks);
            case 66: return std::make_unique<Mapper066>(prgBanks, chrBanks);
            case 206: return std::make_unique<Mapper206>(prgBanks, chrBanks);
            default:
                return nullptr;
            }
        }

        static bool IsSupported(uint8_t mapperNumber) {
            switch (mapperNumber) {
                case 0: return true;
                case 1: return true;
                case 2: return true;
                case 3: return true;
                case 4: return true;
                case 9: return true;
                case 66: return true;
                case 206: return true;
                default: return false;
            }
        }

        static std::string GetMapperName(uint8_t mapperNumber) {
            switch (mapperNumber) {
            case 0:  return "NROM";
            case 1:  return "MMC1 (SxROM)";
            case 2:  return "UxROM";
            case 3:  return "CNROM";
            case 4:  return "MMC3 (TxROM)";
            case 9:  return "MMC2 (PxROM)";
            case 66: return "GxROM";
            case 206: return "Namcot 108 / Tengen MIMIC-1";
            default: return "Unknown Mapper (" + std::to_string(mapperNumber) + ")";
            }
        }

        static std::string GetMapperInfo(uint8_t mapperNumber) {
            switch (mapperNumber) {
            case 0:  return "No mapper - simple direct mapping. 16KB or 32KB PRG, up to 8KB CHR.";
            case 1:  return "Nintendo MMC1. Switchable PRG/CHR banks, serial register loading, 8KB cart RAM.";
            case 2:  return "UxROM. Switchable 16KB PRG banks, fixed CHR.";
            case 3:  return "CNROM. Fixed PRG, switchable 8KB CHR banks.";
            case 4:  return "Nintendo MMC3. Advanced banking, scanline IRQ counter, 8KB cart RAM.";
            case 9:  return "Nintendo MMC2. 16KB PRG banking with special CHR banking for split-screen effects.";
            case 66: return "GxROM. Simple 32KB PRG + 8KB CHR banking.";
            case 206: return "Tengen Tetris", "Gauntlet", "R.B.I. Baseball";
            default: return "No information available.";
            }
        }

        static std::vector<std::string> GetExampleGames(uint8_t mapperNumber) {
            switch (mapperNumber) {
            case 0:  return { "Donkey Kong", "Mario Bros", "Excitebike", "Ice Climber" };
            case 1:  return { "The Legend of Zelda", "Metroid", "Kid Icarus", "Mega Man 2" };
            case 2:  return { "Mega Man", "Castlevania", "Contra", "Duck Tales" };
            case 3:  return { "Solomon's Key", "Arkanoid", "Paperboy", "Cybernoid" };
            case 4:  return { "Super Mario Bros 3", "Mega Man 3-6", "Kirby's Adventure", "Batman" };
            case 9:  return { "Punch-Out!!", "Mike Tyson's Punch-Out!!", "Rad Racer" };
            case 66: return { "Super Mario Bros + Duck Hunt", "Gumshoe" };
            default: return {};
            }
        }
    };
}
