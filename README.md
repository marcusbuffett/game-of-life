# Game of Life with ncurses

This is an implementation of Conway's Game of Life written in Haskell, using ncurses for display.

[![asciicast](https://asciinema.org/a/6btsatbsu46jx2c0t92xtb8fg.png)](https://asciinema.org/a/6btsatbsu46jx2c0t92xtb8fg)

# Installation
  Installing from Hackage requires a working cabal installation (or stack):
  ```bash
  cabal update 
  cabal install game-of-life
  game-of-life # Make sure ~/.cabal/bin is in your path
  ```

# Running / Testing

First, install stack. `curl -sSL https://get.haskellstack.org/ | sh` will work on most Unix systems, otherwise checkout https://docs.haskellstack.org/en/stable/README/ . If you haven't run other Haskell projects on your system, the first time you try to build the project it may say you have to run `stack setup`; do so.

Note that you'll need the ncurses header files on your system. OS X should have these installed by default. Some Linux distros won't, running `sudo apt-get install libncurses-dev` (replace apt-get with package manager of choice) should fix any issues.

Running:
```bash
stack build && stack exec game-of-life
```

Testing:
```bash
stack build && stack test
```

Installing:
```bash
stack build && stack install .
```

# Notes

- Currently doesn't handle resizing the shell during execution
- The screen doesn't update fast enough sometimes, causing tearing / lag / unresponsiveness. Seems to have more to do with ncurses / the shell than the speed of the simulation
- Some linux distros cause the program to crash (and mess up the user's shell since the ncurses cleanup commands don't run)
