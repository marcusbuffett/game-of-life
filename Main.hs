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
-- `do` blocks in Haskell are just syntactic sugar for
-- sequencing monadic values. Somewhat similar to Scala's
-- `for`/`yield` syntax
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
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- Since `window` is in the IO monad, and `life` takes a
  -- Window without any surrounding structure, >>= is used
  -- to run a function on the value "inside" the monad, I
  -- *think* it's comparable to Scala's `flatMap`
  randomBoard height (width-1) >>= life window
  -- Reset invisible cursor, timeout, raw, etc.
  Curses.resetParams
  Curses.endWin

-- Double colons in Haskell are used to specify the type of
-- something. The way to read the following signature is :
-- `life` is a function which takes two arguments, a
-- Curses.Window and a Board, and returns `IO ()`. `IO ()`
-- is an IO action that returns no result (printing to
-- the screen for example). Type signatures aren't required
-- in most cases but are encouraged for top level
-- definitions
life :: Curses.Window -> Board -> IO ()
life window board = do
    _ <- drawBoard board window
    -- Ideally should take into account how long it took to
    -- draw the board
    threadDelay delay
    -- \arg1 arg2 -> arg1 + arg2
    -- is the syntax for a lambda in Haskell
    Curses.getch >>= \ch -> case Curses.decodeKey ch of 
      -- KeyUnknown means no key was pressed
      Curses.KeyUnknown _ -> life window (step board)
      -- `return` in Haskell doesn't mean the same thing it
      -- does in other languages, instead it lifts a value
      -- into a monad. Ex: 
      --
      -- (return 1 :: Maybe Int) == Just 1.  
      --
      -- In this case it's lifting `unit` (empty
      -- parentheses) into the IO monad
      _                   -> return ()

drawBoard :: Board -> Curses.Window -> IO ()
drawBoard board window = do
  Curses.move 0 0
  Curses.wAddStr window (boardRep board)
  Curses.wRefresh window

boardRep :: Board -> String
boardRep board = intercalate "\n" rows
  -- <$> is just an infix version of fmap
  where rows = (concatMap show . elems) <$> elems (bGrid board)

randomBoard :: Int -> Int -> IO Board
randomBoard h w = randomGrid h w >>= \grid -> return $ Board grid h w

randomGrid :: Int -> Int -> IO Grid
-- The reason for all the fmaps in the following functions
-- is that getting a random number requires an IO action, so
-- a lot of operations in these functions have to be
-- "lifted" over the IO Functor
randomGrid h w = listToArray <$> replicateM h randomRow
  where
    randomRow = listToArray <$> randomListOfCells
    -- (fmap . fmap) seems more confusing than it really is.
    -- Two fmaps composed together return a function that
    -- can map a function over two Functors, like:
    --
    -- (fmap . fmap) (+3) [Just 3] == [Just 6]
    --
    -- In that case, the two Functors are List and Maybe,
    -- below the two Functors are IO and List
    randomListOfCells = (fmap . fmap) randomCell (replicateM w randomIO)
    randomCell r
      | r < density  = Alive
      | otherwise    = Dead
