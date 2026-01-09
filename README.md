# NES.Net
- Was going to be Total conversion of https://github.com/OneLoneCoder/olcNES
- Couldent realy Convert that renderer
  - So we have a crude pixel drawing VideoThread drawing to a picturebox.
- Couldent realy convert the Audio (probably could have im just not that interested)
  - We are using NAUDIO NuGet package.
- We are also using System.Memory NuGet package because for some reason its not available by default?

## Status
- Not working
  - Backgrounds Load
  - Players Sprite is Invisible
  - (Arkanoid) Ball is Invisible

## Controls
- "p"
  - View pallete patterns
    - currently disabled
- a, s ,d ,w
  - Left, Down, Right, Up
- j, k, n, m
  - Select, Start, A, B
    
## Notes
my testing has been mostly with Arkanoid (which almost works...).
