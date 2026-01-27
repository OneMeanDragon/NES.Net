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

## Upcoming
- Battery Backed Save Files
  - The cartridge interface 
    - will create the save file if it is missing.
    - will auto load it if present at opening the cartridge.
    - will auto save it upon stoping and or closing the application.
      - keeping this in mind, back up your own save files as needed this part is not fool proof with crashes..

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
