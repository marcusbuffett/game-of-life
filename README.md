# Game of Life with ncurses

This is an implementation of Conway's Game of Life written in Haskell and using ncurses for display.

# Installation

You'll need haskell and cabal on your system (`brew install cabal-install` on OS X). Otherwise check out https://wiki.haskell.org/Cabal-Install#Installation.

Run the following if you have cabal installed :

    cabal update && cabal install game-of-life

If that doesn't work for any reason, you can try cloning this repo and running :

    stack build && stack install .

# Usage

To watch the game of life after installation: 

    game-of-life

# Notes

- Currently doesn't handle resizing the shell during execution
- The screen doesn't update fast enough sometimes, causing tearing / lag / unresponsiveness. Seems to have more to do with ncurses / the shell than the speed the program executes.
