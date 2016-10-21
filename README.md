# Game of Life with ncurses

This is an implementation of Conway's Game of Life written in Haskell, using ncurses for display.

# Installation

### If you have cabal
  ```bash
  cabal update 
  cabal install game-of-life
  game-of-life # Make sure ~/.cabal/bin is in your path
  ```

### From source

First, install stack. `curl -sSL https://get.haskellstack.org/ | sh` will work on most Unix systems, otherwise checkout https://docs.haskellstack.org/en/stable/README/ .

```bash
stack build && stack install .
```

# Tests

To run the test suite:

```bash
stack build && stack test
```

# Running

To build and run the project:

```bash
stack build && stack exec game-of-life
```

# Notes

- Currently doesn't handle resizing the shell during execution
- The screen doesn't update fast enough sometimes, causing tearing / lag / unresponsiveness. Seems to have more to do with ncurses / the shell than the speed of the simulation.
