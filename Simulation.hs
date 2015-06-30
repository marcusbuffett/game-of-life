{-# LANGUAGE OverloadedStrings #-}
module Simulation where
import Data.Array

data State = Alive | Dead deriving (Eq)
type Row = Array Int State
type Grid = Array Int Row

data Board = Board {bGrid :: Grid, bRows :: Int, bCols :: Int} deriving (Show)
data Coord = Coord Int Int deriving (Show)

instance Num Coord where
  Coord a b + Coord c d = Coord (a+c) (b+d)
  Coord a b - Coord c d = Coord (a-c) (b-d)

instance Show State where
  show Alive = "■"
  show Dead = " "

directions :: [Coord]
directions = [Coord y x | y <- [-1..1], x <- [-1..1], not (x == 0 && y == 0)]

neighbors :: Board -> Coord -> [Coord]
neighbors board coord = filter (\c -> state board c == Alive) validCoords
  where
    validCoords = filter (validCoord board) coordsAround
    coordsAround = map (+ coord) directions

validCoord :: Board -> Coord -> Bool
validCoord board (Coord y x) = xValid && yValid
  where 
    xValid = x >= 0 && x < bCols board
    yValid = y >= 0 && y < bRows board

numNeighbors :: Board -> Coord -> Int
numNeighbors board coord = length $ neighbors board coord

state :: Board -> Coord -> State
state board (Coord y x) = (bGrid board) ! y ! x

singleStep :: Board -> Coord -> State
singleStep board coord = stepCell currentState bors
  where 
    currentState = state board coord
    bors = numNeighbors board coord

stepCell :: State -> Int -> State
stepCell Dead 3 = Alive
stepCell Dead _ = Dead
stepCell Alive n
  | n < 2 = Dead
  | n > 3 = Dead
  | otherwise = Alive

step :: Board -> Board
step board = board {bGrid = newGrid}
  where
    newGrid :: Grid
    newGrid = array (0, bRows board-1) $ zip [0..bCols board-1] (map stepRow [0..bRows board-1])
    stepRow :: Int -> Row
    stepRow y = array (0,bCols board-1) $ zip [0..bCols board - 1] [singleStep board (Coord y x) | x <- [0..(bCols board)-1]]
