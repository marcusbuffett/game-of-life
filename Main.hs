module Main where

import Simulation

import qualified UI.HSCurses.Curses as Curses
import Control.Monad(replicateM)
import System.Random
import Control.Concurrent(threadDelay)
import Data.List(intercalate)
import Data.Array

-- Delay between iterations (μs)
delay :: Int
delay = 50000

-- Probability of a cell being Alive on board generation
density :: Float
density = 0.37

main :: IO ()
main = do
  window <- Curses.initScr
  -- similar to cbreak mode, keys are passed immediately
  -- (includes control-flow keys like ^-C, ^-D, etc)
  -- If control-flow keys weren't passed through, ncurses
  -- cleanup commands like resetParams and endWin wouldn't
  -- be run on interrupt
  Curses.raw True
  _ <- Curses.cursSet Curses.CursorInvisible
  Curses.timeout 0
  -- Get screen size for board
  (height, width) <- Curses.scrSize
  randomBoard height (width-1) >>= life window
  -- Reset invisible cursor, timeout, raw, etc.
  Curses.resetParams
  Curses.endWin

life :: Curses.Window -> Board -> IO ()
life window board = do
    _ <- drawBoard board window
    -- Ideally should take into account how long it took to
    -- draw the board
    threadDelay delay
    Curses.getch >>= \ch -> case Curses.decodeKey ch of 
      -- KeyUnknown means no key was pressed
      Curses.KeyUnknown _ -> life window (step board)
      _                   -> return ()

drawBoard :: Board -> Curses.Window -> IO ()
drawBoard board window = do
  Curses.move 0 0
  Curses.wAddStr window (boardRep board)
  Curses.wRefresh window

boardRep :: Board -> String
boardRep board = intercalate "\n" rows
  where rows = (concatMap show . elems) <$> elems (bGrid board)

randomBoard :: Int -> Int -> IO Board
randomBoard h w = randomGrid h w >>= \grid -> return $ Board grid h w

randomGrid :: Int -> Int -> IO Grid
randomGrid h w = listToArray <$> replicateM h randomRow
  where
    randomRow = listToArray <$> randomListOfCells
    randomListOfCells = (fmap . fmap) randomCell (replicateM w randomIO)
    randomCell r
      | r < density  = Alive
      | otherwise    = Dead
