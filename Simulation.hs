module Simulation where

-- Chose array for it's constant time random access
-- (Haskell's List has linear time random access)
import Data.Array
import Control.Applicative(liftA2)

-- Define a sum type, State, which has two constructors:
-- Alive and Dead. Automatically derive the Eq instance
data State = Alive | Dead deriving (Eq)

-- Define type aliases for common types, to make the type
-- signatures a bit more sane
--
-- Haskell uses spaces for parameterized types, so 
-- (Array Int State) in Haskell is comparable to 
-- (Array[Int, State]) in Scala (Array in Scala doesn't take
-- two type arguments, just translating the syntax)
type Row = Array Int State
type Grid = Array Int Row

-- Create a Board type, automatically deriving the Show
-- instance
data Board = Board {bGrid :: Grid, bHeight :: Int, bWidth :: Int} deriving (Show)
-- Create a Coord type, automatically deriving the Show and
-- Eq instances
data Coord = Coord Int Int deriving (Show, Eq)

-- Make State an instance of the Show typeclass, which will
-- be used for displaying the board
instance Show State where
  show Alive = "■"
  show Dead  = " "

-- Haskell allows for the definition of functions with
-- non-alphanumeric characters, the only difference being
-- that non-alphanumeric functions are infix by default
-- (although prefix notation can be used if infix notation
-- isn't desired)
(<+>) :: Coord -> Coord -> Coord
Coord a b <+> Coord c d = Coord (a + c) (b + d)

-- Gets the number of alive cells around a coordinate
neighbors :: Board -> Coord -> Int
-- Haskell functions support currying by default
--
-- (==) and (state) are both functions with an arity of 2,
-- applying one argument returns a new function, with an
-- arity of 1
--
-- (==)          :: State -> State -> Bool
-- (== Alive)    :: State -> Bool
-- (state)       :: Board -> Coord -> State
-- (state board) :: Coord -> State
--
-- Composing the two partially applied functions gives a
-- type of Coord -> Bool, which is the type that `filter`
-- expects
--
-- Using partially applied functions as arguments to
-- higher-order functions is a common pattern in Haskell
neighbors board coord = length $ filter ((== Alive) . state board) validCoords
  where
  validCoords = filter (validCoord board) coordsAround
  coordsAround = map (<+> coord) directions
  -- liftA2 takes a function and two Applicatives, and the
  -- function over the two Applicatives, for example:
  --
  -- liftA2 (*) (Just 3) (Just 14) = Just 42
  --
  -- Because lists in Haskell are instances of the
  -- Applicative typeclass, liftA2 can be used to lift a
  -- function over lists :
  --
  -- liftA2 (*) [1,2] [3,4] = [3,4,6,8]
  --
  -- The same principle is used here to lift the Coord
  -- constructor over two lists, returning a list of Coords
  directions = filter (/= Coord 0 0) $ liftA2 Coord [-1..1] [-1..1]

-- Determines whether a given coordinate is in bounds
validCoord :: Board -> Coord -> Bool
-- Uses pattern matching to get the x and y values of a
-- Coord, Haskell has similar pattern matching to Scala
-- afaik
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
-- Uses pattern matching on State and Int (# of alive neighbors)
stepCell Dead  3 = Alive
stepCell Dead  _ = Dead
stepCell Alive 2 = Alive
stepCell Alive 3 = Alive
stepCell Alive _ = Dead

-- Runs one iteration of game of life
step :: Board -> Board
-- board { bGrid = ... } is a way of creating a new Board
-- with all the same fields as `board`, except bGrid, like
-- updating a single property of an instance, except it
-- returns a new instance with the one thing changed
--
-- `uncurry` takes a function of type (a -> b -> c) and
-- turns it into a function of type ((a,b) -> c), in this
-- case it uncurries updateRow from a binary function of
-- type (Int -> Array Int a -> Row), to a unary function of
-- type ((Int, Array Int a) -> Row)
step board = board { bGrid = mapIndices (uncurry updateRow) $ bGrid board }
  where
  updateRow y = mapIndices (nextState board . Coord y . fst)
  -- The following function:
  -- f x y = (+) x y
  -- can also be written as:
  -- f x = (+) x
  -- and can actually be made "pointfree":
  -- f = (+)
  -- These three functions are all equivalent in Haskell
  --
  -- The below function could be written as :
  -- mapIndices f arr = listToArray . fmap f $ assocs arr
  --
  -- Omitting arguments is called eta reduction. Using eta
  -- reduction to make functions more concise and readable
  -- is generally encouraged in Haskell. Of course, it can
  -- be taken too far
  --
  -- In this case the second argument of mapIndices is not
  -- written. It is still a binary function, but it may be
  -- more correct to think of it as a unary function that
  -- returns another unary function
  mapIndices :: ((Int, a) -> b) -> Array Int a -> Array Int b
  mapIndices f = listToArray . fmap f . assocs

listToArray :: [a] -> Array Int a
listToArray list = listArray (0, length list - 1) list
