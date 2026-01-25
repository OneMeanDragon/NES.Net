#include <iostream>
#include <string>
#include <cstring>

// Include your component headers
#include "CPU/CPU6502.h"
#include "PPU/PPU2C02.h"
#include "APU/APU2A03.h"

// Include diagnostic framework
#include "DiagnosticTest.h"
#include "DiagnosticCartridge.h"
#include "DiagnosticBus.h"

// Include test suites
#include "CPUTests.h"
#include "PPUTests.h"
#include "APUTests.h"

void PrintHeader(const std::string& title) {
    std::cout << "\n|----------------------------------------|" << std::endl;
    std::cout << "|";
    size_t padding = (40 - title.length()) / 2;
    for (size_t i = 0; i < padding; i++) std::cout << " ";
    std::cout << title;
    for (size_t i = 0; i < 40 - padding - title.length(); i++) std::cout << " ";
    std::cout << "|" << std::endl;
    std::cout << "|----------------------------------------|" << std::endl;
}

void PrintUsage() {
    std::cout << "NES Component Diagnostics Tool\n" << std::endl;
    std::cout << "Usage:" << std::endl;
    std::cout << "  nes_diagnostics [options]" << std::endl;
    std::cout << "\nOptions:" << std::endl;
    std::cout << "  --cpu              Run CPU tests only" << std::endl;
    std::cout << "  --ppu              Run PPU tests only" << std::endl;
    std::cout << "  --apu              Run APU tests only" << std::endl;
    std::cout << "  --nestest <path>   Run nestest.nes ROM (provide path)" << std::endl;
    std::cout << "  --all              Run all tests (default)" << std::endl;
    std::cout << "  --help             Show this help message" << std::endl;
    std::cout << "\nExamples:" << std::endl;
    std::cout << "  nes_diagnostics" << std::endl;
    std::cout << "  nes_diagnostics --cpu" << std::endl;
    std::cout << "  nes_diagnostics --nestest nestest.nes" << std::endl;
}

int main(int argc, char* argv[]) {
    bool runCPU = false;
    bool runPPU = false;
    bool runAPU = false;
    bool runNESTest = true;
    std::string nestestPath = "nestest.nes";

    // Parse command line arguments
    //if (argc == 1) {
    //    // No arguments, run everything except nestest
    //    runCPU = runPPU = runAPU = true;
    //}
    //else {
    //    for (int i = 1; i < argc; i++) {
    //        std::string arg = argv[i];
    //        if (arg == "--help" || arg == "-h") {
    //            PrintUsage();
    //            return 0;
    //        }
    //        else if (arg == "--cpu") {
    //            runCPU = true;
    //        }
    //        else if (arg == "--ppu") {
    //            runPPU = true;
    //        }
    //        else if (arg == "--apu") {
    //            runAPU = true;
    //        }
    //        else if (arg == "--all") {
    //            runCPU = runPPU = runAPU = true;
    //        }
    //        else if (arg == "--nestest") {
    //            runCPU = true;  // Also run CPU tests
    //            runNESTest = true;
    //            if (i + 1 < argc) {
    //                nestestPath = argv[++i];
    //            }
    //            else {
    //                std::cerr << "Error: --nestest requires a path argument" << std::endl;
    //                return 1;
    //            }
    //        }
    //        else {
    //            std::cerr << "Unknown option: " << arg << std::endl;
    //            PrintUsage();
    //            return 1;
    //        }
    //    }
    //}

    std::cout << "========================================" << std::endl;
    std::cout << "   NES COMPONENT DIAGNOSTICS v1.0" << std::endl;
    std::cout << "========================================" << std::endl;

    DiagnosticTest test;

    // Create components
    CPU6502* cpu = new CPU6502();
    PPU2C02* ppu = new PPU2C02();
    APU2A03* apu = new APU2A03();
    DiagnosticBus* bus = new DiagnosticBus();
    DiagnosticCartridge* cart = new DiagnosticCartridge();

    // Connect everything
    bus->ConnectCPU(cpu);
    bus->ConnectPPU(ppu);
    bus->ConnectAPU(apu);
    bus->ConnectCartridge(cart);

    cpu->ConnectBus(bus);
    ppu->ConnectBus(bus);
    ppu->SetCartridge(cart);
    apu->ConnectBus(bus);
    apu->SetCartridge(cart);

    // Run CPU tests
    if (runCPU) {
        PrintHeader("CPU6502 DIAGNOSTICS");

        TestCPUBasics(test, cpu, bus);
        TestCPULoadStore(test, cpu, bus, cart);
        TestCPUArithmetic(test, cpu, bus, cart);
        TestCPULogical(test, cpu, bus, cart);
        TestCPUShifts(test, cpu, bus, cart);
        TestCPUFlags(test, cpu, bus, cart);
        TestCPUBranching(test, cpu, bus, cart);
        TestCPUStackOperations(test, cpu, bus, cart);
    }

    // Run NESTest ROM if requested
    if (runNESTest) {
        PrintHeader("NESTEST.NES VALIDATION");

        // Need to reset and reconnect for nestest
        delete cart;
        cart = new DiagnosticCartridge();
        bus->ConnectCartridge(cart);
        ppu->SetCartridge(cart);
        apu->SetCartridge(cart);

        //cpu->Reset(true);
        TestCPUWithNESTestROM(test, cpu, bus, cart, nestestPath);
    }

    // Run PPU tests
    if (runPPU) {
        PrintHeader("PPU2C02 DIAGNOSTICS");

        TestPPUBasics(test, ppu);
        TestPPURegisters(test, ppu);
        TestPPUMemory(test, ppu);
        TestPPUScrolling(test, ppu);
        TestPPUOAM(test, ppu);
        TestPPUVBlank(test, ppu);
        TestPPUPatternTables(test, ppu, cart);
        TestPPUNametables(test, ppu);
        TestPPUPalettes(test, ppu);
        TestPPUAddressIncrement(test, ppu);
    }

    // Run APU tests
    if (runAPU) {
        PrintHeader("APU2A03 DIAGNOSTICS");

        TestAPUBasics(test, apu);
        TestAPUStatusRegister(test, apu);
        TestAPUPulse1(test, apu);
        TestAPUPulse2(test, apu);
        TestAPUTriangle(test, apu);
        TestAPUNoise(test, apu);
        TestAPUSampleGeneration(test, apu);
        TestAPUFrameCounter4Step(test, apu);
        TestAPUFrameCounter5Step(test, apu);
        TestAPUFrameIRQ(test, apu);
        TestAPUEnvelope(test, apu);
        TestAPUSweep(test, apu);
        TestAPULengthCounter(test, apu);
        TestAPUAllChannelsMixed(test, apu);
    }

    // Print summary
    test.PrintSummary();

    // Additional info
    if (runNESTest) {
        std::cout << "\n========================================" << std::endl;
        std::cout << "NESTEST VALIDATION NOTES:" << std::endl;
        std::cout << "========================================" << std::endl;
        std::cout << "The nestest.nes ROM log has been written to:" << std::endl;
        std::cout << "  nestest_output.log" << std::endl;
        std::cout << "\nTo validate your CPU implementation:" << std::endl;
        std::cout << "1. Download the official nestest.log from:" << std::endl;
        std::cout << "   https://www.qmtpro.com/~nes/misc/nestest.log" << std::endl;
        std::cout << "2. Compare nestest_output.log with nestest.log" << std::endl;
        std::cout << "3. They should match exactly for a correct implementation" << std::endl;
        std::cout << "\nYou can use diff tools to compare:" << std::endl;
        std::cout << "  diff nestest_output.log nestest.log" << std::endl;
        std::cout << "  (or use your favorite diff tool)" << std::endl;
    }

    // Cleanup
    delete cart;
    delete bus;
    delete apu;
    delete ppu;
    delete cpu;

    std::cout << "\n========================================" << std::endl;
    std::cout << "Diagnostics completed." << std::endl;
    std::cout << "========================================" << std::endl;

    return (test.GetFailed() == 0) ? 0 : 1;
}