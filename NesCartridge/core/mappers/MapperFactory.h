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
            case Mapper000::ID: return std::make_unique<Mapper000>(prgBanks, chrBanks);
            case Mapper001::ID: return std::make_unique<Mapper001>(prgBanks, chrBanks);
            case Mapper002::ID: return std::make_unique<Mapper002>(prgBanks, chrBanks);
            case Mapper003::ID: return std::make_unique<Mapper003>(prgBanks, chrBanks);
            case Mapper004::ID: return std::make_unique<Mapper004>(prgBanks, chrBanks);
            case Mapper009::ID: return std::make_unique<Mapper009>(prgBanks, chrBanks);
            case Mapper066::ID: return std::make_unique<Mapper066>(prgBanks, chrBanks);
            case Mapper206::ID: return std::make_unique<Mapper206>(prgBanks, chrBanks);
            default:
                return nullptr;
            }
        }

        static bool IsSupported(uint8_t mapperNumber) {
            switch (mapperNumber) {
                case Mapper000::ID: return true;
                case Mapper001::ID: return true;
                case Mapper002::ID: return true;
                case Mapper003::ID: return true;
                case Mapper004::ID: return true;
                case Mapper009::ID: return true;
                case Mapper066::ID: return true;
                case Mapper206::ID: return true;
                default: return false;
            }
        }

        static std::string GetMapperName(uint8_t mapperNumber) {
            switch (mapperNumber) {
            case Mapper000::ID: return Mapper000::NAME;
            case Mapper001::ID: return Mapper001::NAME;
            case Mapper002::ID: return Mapper002::NAME;
            case Mapper003::ID: return Mapper003::NAME;
            case Mapper004::ID: return Mapper004::NAME;
            case Mapper009::ID: return Mapper009::NAME;
            case Mapper066::ID: return Mapper066::NAME;
            case Mapper206::ID: return Mapper206::NAME;
            default: return "Unknown Mapper (" + std::to_string(mapperNumber) + ")";
            }
        }

        static std::string GetMapperInfo(uint8_t mapperNumber) {
            switch (mapperNumber) {
            case Mapper000::ID: return "No mapper - simple direct mapping. 16KB or 32KB PRG, up to 8KB CHR.";
            case Mapper001::ID: return "Nintendo MMC1. Switchable PRG/CHR banks, serial register loading, 8KB cart RAM.";
            case Mapper002::ID: return "UxROM. Switchable 16KB PRG banks, fixed CHR.";
            case Mapper003::ID: return "CNROM. Fixed PRG, switchable 8KB CHR banks.";
            case Mapper004::ID: return "Nintendo MMC3. Advanced banking, scanline IRQ counter, 8KB cart RAM.";
            case Mapper009::ID: return "Nintendo MMC2. 16KB PRG banking with special CHR banking for split-screen effects.";
            case Mapper066::ID: return "GxROM. Simple 32KB PRG + 8KB CHR banking.";
            case Mapper206::ID: return "Tengen Tetris", "Gauntlet", "R.B.I. Baseball";
            default:  return "No information available.";
            }
        }

        static std::vector<std::string> GetExampleGames(uint8_t mapperNumber) {
            switch (mapperNumber) {
            case Mapper000::ID: return { "Donkey Kong", "Mario Bros", "Excitebike", "Ice Climber" };
            case Mapper001::ID: return { "The Legend of Zelda", "Metroid", "Kid Icarus", "Mega Man 2" };
            case Mapper002::ID: return { "Mega Man", "Castlevania", "Contra", "Duck Tales" };
            case Mapper003::ID: return { "Solomon's Key", "Arkanoid", "Paperboy", "Cybernoid" };
            case Mapper004::ID: return { "Super Mario Bros 3", "Mega Man 3-6", "Kirby's Adventure", "Batman" };
            case Mapper009::ID: return { "Punch-Out!!", "Mike Tyson's Punch-Out!!", "Rad Racer" };
            case Mapper066::ID: return { "Super Mario Bros + Duck Hunt", "Gumshoe" };
            case Mapper206::ID: return { "RBI. Baseball" };
            default: return {};
            }
        }
    };
}
