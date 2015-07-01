module Main where

import Simulation

import qualified UI.HSCurses.Curses as Curses
import Control.Monad(replicateM)
import System.Random
import Control.Concurrent(threadDelay)
import Data.List(intersperse)
import Data.Array

delay :: Int
delay = 50000
density :: Float
density = 0.37

main :: IO ()
main = do
  window <- Curses.initScr
  Curses.raw True
  _ <- Curses.cursSet Curses.CursorInvisible
  Curses.timeout 0
  (rows, cols) <- Curses.scrSize
  randomBoard (rows) (cols-1) >>= life window
  Curses.resetParams
  Curses.endWin

life :: Curses.Window -> Board -> IO ()
life window board = do
    _ <- drawBoard board window
    threadDelay delay
    Curses.getch >>= \ch -> case Curses.decodeKey ch of 
      Curses.KeyUnknown _ -> life window (step board)
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
  rowStrings r = concat [show (grid ! r ! c) | c <- [0..cs-1]]
  rs = bRows board 
  cs = bCols board 
  grid = bGrid board

randomBoard :: Int -> Int -> IO Board
randomBoard rows cols = randomGrid rows cols >>= 
                                \grid -> return $ Board grid rows cols

randomGrid :: Int -> Int -> IO Grid
randomGrid rs cs = replicateM cs randomRow >>= \rows -> return $ listArray (0, rs-1) rows
  where
  randomRow :: IO (Array Int State)
  randomRow = randomListOfCells >>= \cells -> return $ listArray (0, cs-1) cells

  randomListOfCells :: IO [State]
  randomListOfCells = replicateM cs randomIO >>= return . map randomCell

  randomCell :: Float -> State
  randomCell r
    | r < density  = Alive
    | r >= density = Dead
    | otherwise    = Dead

