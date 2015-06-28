{-# LANGUAGE OverloadedStrings #-}

module Main where
import Simulation
import Control.Monad(forever, replicateM)
import Control.Concurrent(threadDelay)
import qualified UI.HSCurses.CursesHelper as CursesH
import qualified UI.HSCurses.Curses as Curses
import System.Random
import Data.List(intersperse)

exampleBoard :: Grid
exampleBoard = [[Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Alive, Dead , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Alive , Alive , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Alive , Dead , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead]  ,
                [Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead , Dead]]

randomboard :: Int -> Int -> Float -> IO Board
randomboard rows cols density = replicateM rows randomRow >>= \grid -> return $ Board {bGrid = grid, bRows = rows, bCols = cols}
  where
    randomRow :: IO [State]
    randomRow = replicateM cols (randomIO >>= randomCell)
    randomCell :: Float -> IO State
    randomCell r
      | r < density  = return Alive
      | r >= density = return Dead
      | otherwise    = return Dead
  

main :: IO ()
main = do
  window <- Curses.initScr
  _ <- Curses.cursSet Curses.CursorInvisible
  (lines, cols) <- Curses.scrSize
  randomboard (lines-1) (cols-1) 0.375 >>= (life 0 window)
  _ <- Curses.cursSet Curses.CursorVisible
  Curses.refresh
  CursesH.end
  print cols
  print lines
  {- _ <- Curses.cursSet Curses.CursorVisible -}

drawBoard :: Board -> Curses.Window -> IO ()
drawBoard board window = do
  Curses.move 0 0
  Curses.wAddStr window (boardRep board)
  Curses.wRefresh window
  {- print $ boardRep board -}

boardRep :: Board -> String
boardRep board = concat . intersperse "\n" $ map rowStrings (bGrid board)
  where
  rowStrings row = concat $ map show row

drawRow :: Curses.Window -> String -> IO ()
drawRow window row = do
  Curses.wAddStr window row 
  {- (y, x) <- Curses.getYX window -}
  {- Curses.move (y) x -}
  
life :: Int -> Curses.Window -> Board -> IO ()
life gen window board
  | gen > 1000 = return ()
  | otherwise = do
    _ <- drawBoard board window
    {- threadDelay 2000000 -}
    life (gen+1) window (step board)


rowStrings :: Board -> [String]
rowStrings board = map rowString (bGrid board)
  where
    rowString :: [State] -> String
    rowString row = (concat $ map show row) ++ "\n"
