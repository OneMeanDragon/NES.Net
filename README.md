# NES.Net
- Required Packages from NuGet
  - System.Memory
- FMOD Sdk
  - This is in the vendor folder of the NesChipset C++ project
- NesCartridge (currently you must compile this, still not optimal..)
- NesChipset (Required compile, requires NesCartridge is compiled, also requires the FMOD sdk)

## Status
- Working (still slow*ish until its not..)
  - (NESTEST) (you will find it here https://www.nesdev.org/wiki/Emulator_tests)
  - (Arkanoid)
  - Some others as well however still loads of bugs
- Knowen Crashes
  - Stop the emulator before closing the window (thread issue, may have been corrected)

## Controls
- "p"
  - View pallete patterns
    - currently disabled
- a, s ,d ,w
  - Left, Down, Right, Up
- j, k, n, m
  - Select, Start, A, B
    
## Notes
my testing has been mostly with Arkanoid.
