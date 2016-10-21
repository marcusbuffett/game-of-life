module Simulation where

-- Chose array for it's constant time random access
-- (Haskell's List has linear time random access)
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

-- Gets the coordinates of the cells that are alive around a
-- coordinate
neighbors :: Board -> Coord -> [Coord]
neighbors board coord = filter (\c -> state board c == Alive) validCoords
  where
  validCoords = filter (validCoord board) coordsAround
  coordsAround = map (+ coord) directions
  directions = [Coord y x | y <- [-1..1], x <- [-1..1], not (x==0 && y==0)]

-- Determines whether a given coordinate is in bounds
validCoord :: Board -> Coord -> Bool
validCoord board (Coord y x) = xValid && yValid
  where 
  xValid = x >= 0 && x < bCols board
  yValid = y >= 0 && y < bRows board

-- Get number of neighbors 
numNeighbors :: Board -> Coord -> Int
numNeighbors board coord = length $ neighbors board coord

-- Gets the state of a coordinate on the board 
-- (TODO: make this a total function)
state :: Board -> Coord -> State
state board (Coord y x) = bGrid board ! y ! x

-- Executes a single step for a coordinate on the board
singleStep :: Board -> Coord -> State
singleStep board coord = stepCell currentState bors
  where 
  currentState = state board coord
  bors = numNeighbors board coord

-- Given the current state, and the # of alive neighbors,
-- returns the new state of the cell
stepCell :: State -> Int -> State
stepCell Dead 3 = Alive
stepCell Dead _ = Dead
stepCell Alive n
  | n < 2 = Dead
  | n > 3 = Dead
  | otherwise = Alive

-- Runs one iteration of game of life
step :: Board -> Board
step board = board {bGrid = newGrid}
  where
    newGrid :: Grid
    newGrid = listArray (0, rows-1) $ map newRow [0..rows-1]

    newRow :: Int -> Row
    newRow y = listArray (0,cols-1)
                  [singleStep board (Coord y x) | x <- [0..cols-1]]

    cols = bCols board
    rows = bRows board 
