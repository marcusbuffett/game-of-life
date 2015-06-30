module Main where

{-# LANGUAGE OverloadedStrings #-}

import Simulation
import Control.Monad(replicateM)
import Control.Concurrent(threadDelay)
import qualified UI.HSCurses.CursesHelper as CursesH
import qualified UI.HSCurses.Curses as Curses
import System.Random
import Data.List(intersperse)
import Data.Array

delay :: Int
density :: Float
gens :: Int
delay = 0
density = 0.37
gens = 500

randomBoard :: Int -> Int -> IO Board
randomBoard rows cols = randomGrid rows cols >>= 
                                \grid -> return $ Board {bGrid = grid, bRows = rows, bCols = cols}
randomGrid :: Int -> Int -> IO Grid
randomGrid rs cs = replicateM cs randomRow >>= \rows -> return $ array (0, rs-1) (zip [0..rs-1] rows)
  where
  randomRow :: IO (Array Int State)
  randomRow = randomListOfCells >>= \cells -> return $ array (0,cs-1) (zip [0..cs-1] cells)
  randomListOfCells = replicateM cs (randomIO >>= randomCell)
  randomCell :: Float -> IO State
  randomCell r
    | r < density  = return Alive
    | r >= density = return Dead
    | otherwise    = return Dead


main :: IO ()
main = do
  window <- Curses.initScr
  _ <- Curses.cursSet Curses.CursorInvisible
  _ <- Curses.raw True
  (rows, cols) <- Curses.scrSize
  randomBoard (rows-1) (cols-1) >>= (life 0 window)
  Curses.resetParams
  CursesH.end

life :: Int -> Curses.Window -> Board -> IO ()
life gen window board
  | gen > gens = return ()
  | otherwise = do
    _ <- drawBoard board window
    Curses.timeout 0
    threadDelay delay
    Curses.getch >>= \ch -> case Curses.decodeKey ch of 
      Curses.KeyUnknown _ -> life (gen+1) window (step board)
      _                   -> return ()

drawBoard :: Board -> Curses.Window -> IO ()
drawBoard board window = do
  Curses.move 0 0
  Curses.wAddStr window (boardRep board)
  Curses.wRefresh window

boardRep :: Board -> String
boardRep board = concat . intersperse "\n" $ [rowStrings r | r <- [0..rs-1]]
  where
  rowStrings :: Int -> String
  rowStrings r = concat . intersperse "" $ [show (grid ! r ! c) | c <- [0..cs-1]]
  rs = bRows board 
  cs = bCols board 
  grid = bGrid board
