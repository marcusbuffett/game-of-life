module Simulation where

-- Chose array for it's constant time random access
-- (Haskell's List has linear time random access)
import Data.Array
import Control.Applicative(liftA2)

data State = Alive | Dead deriving (Eq)
type Row = Array Int State
type Grid = Array Int Row

data Board = Board {bGrid :: Grid, bHeight :: Int, bWidth :: Int} deriving (Show)
data Coord = Coord Int Int deriving (Show, Eq)

instance Show State where
  show Alive = "■"
  show Dead  = " "

(<+>) :: Coord -> Coord -> Coord
Coord a b <+> Coord c d = Coord (a + c) (b + d)

-- Gets the number of alive cells around a coordinate
neighbors :: Board -> Coord -> Int
neighbors board coord = length $ filter ((== Alive) . state board) validCoords
  where
  validCoords = filter (validCoord board) coordsAround
  coordsAround = map (<+> coord) directions
  directions = filter (/= Coord 0 0) $ liftA2 Coord [-1..1] [-1..1]

-- Determines whether a given coordinate is in bounds
validCoord :: Board -> Coord -> Bool
validCoord board (Coord y x) = xValid && yValid
  where 
  xValid = x >= 0 && x < bWidth board
  yValid = y >= 0 && y < bHeight board

-- Gets the state of a coordinate on the board 
-- (TODO: make this a total function)
state :: Board -> Coord -> State
state board (Coord y x) = bGrid board ! y ! x

-- Executes a single step for a coordinate on the board
nextState :: Board -> Coord -> State
nextState board coord = stepCell currentState bors
  where 
  currentState = state board coord
  bors = neighbors board coord

-- Given the current state, and the # of alive neighbors,
-- returns the new state of the cell
stepCell :: State -> Int -> State
stepCell Dead  3 = Alive
stepCell Dead  _ = Dead
stepCell Alive 2 = Alive
stepCell Alive 3 = Alive
stepCell Alive _ = Dead

-- Runs one iteration of game of life
step :: Board -> Board
step board = board { bGrid = mapIndices (uncurry updateRow) $ bGrid board }
  where
  updateRow y = mapIndices (nextState board . Coord y . fst)
  mapIndices :: ((Int, a) -> b) -> Array Int a -> Array Int b
  mapIndices f = listToArray . fmap f . assocs

listToArray :: [a] -> Array Int a
listToArray list = listArray (0, length list - 1) list
