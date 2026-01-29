#pragma once

namespace CPU {
    constexpr double MHZ = 1000000.0;

    constexpr double NTSC_MASTER_CRYSTAL_MHZ = 21.477272;
    constexpr double  PAL_MASTER_CRYSTAL_MHZ = 26.601712;

    constexpr double NTSC_MASTER_CLOCK_HZ = NTSC_MASTER_CRYSTAL_MHZ * MHZ;
    constexpr double  PAL_MASTER_CLOCK_HZ = PAL_MASTER_CRYSTAL_MHZ * MHZ;

    constexpr double CPU_CLOCK_HZ = (NTSC_MASTER_CLOCK_HZ / 12); // NTSC master hz frequencys
    constexpr double PPU_CLOCK_HZ = (NTSC_MASTER_CLOCK_HZ / 4);
    constexpr double PAL_CPU_CLOCK_HZ = (PAL_MASTER_CLOCK_HZ / 16); // PAL master hz frequencys
    constexpr double PAL_PPU_CLOCK_HZ = (PAL_MASTER_CLOCK_HZ / 5);
}
