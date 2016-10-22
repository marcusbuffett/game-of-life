module Main where

import Test.Hspec
import Simulation
import Data.Array

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "stepCell" $ do
    it "revives dead cells if num_neighbors is 3" $ do
      (stepCell Dead 3) `shouldBe` Alive

    it "does not revive dead cells ∀ num_neighbors in [0,3) ∪ (3,8]" $ do
      mapM_ (\x -> (stepCell Dead x) `shouldBe` Dead) ([0..2] ++ [4..8])

    it "kills live cells ∀ num_neighbors in [0,1] ∪ [4,8]" $ do
      mapM_ (\x -> (stepCell Alive x) `shouldBe` Dead) ([0..1] ++ [4..8])

    it "keeps cells alive ∀ num_neighbors in [2,3]" $ do
      mapM_ (\x -> (stepCell Alive x) `shouldBe` Alive) [2,3]

  describeStep

describeStep :: Spec
describeStep = do
  describe "step" $ do
    it "should handle a very simple board" $ do
      let start = [ 
                    [0, 0, 0],
                    [0, 1, 0],
                    [0, 0, 0] 
                  ]
      let expected_end = [
                    [0, 0, 0],
                    [0, 0, 0],
                    [0, 0, 0] 
                  ]
      shouldBe True $ 
        test_game start 1 expected_end
                  
    it "should progress correctly" $ do
      let zeroth_step = [
                      [0, 1, 0, 1],
                      [1, 1, 1, 1],
                      [0, 1, 0, 1] 
                    ]
      let first_step = [
                     [1, 1, 0, 1],
                     [1, 0, 0, 1],
                     [1, 1, 0, 1] 
                   ]
      let second_step = [
                     [1, 1, 1, 0],
                     [0, 0, 0, 1],
                     [1, 1, 1, 0] 
                   ]

      let board = toBoard zeroth_step
      let two_steps = fmap toTestFormat $ (take 3 . iterate step) board
      two_steps `shouldBe` [zeroth_step, first_step, second_step]


test_game :: [[Int]] -> Int -> [[Int]] -> Bool
test_game input n expected = toTestFormat (iterate step board !! n) == expected
  where
    board = toBoard input

-- Everything below is glue to convert to and from the test
-- format (which is easier to type and see visually), and
-- the actual type of the board

toTestFormat :: Board -> [[Int]]
toTestFormat board = (fmap . fmap) stateToInt $ elems (fmap elems (bGrid board))

toBoard :: [[Int]] -> Board
toBoard input = Board {bGrid = grid, bRows = rows, bCols = cols}
  where
    grid = listArray (0, length input-1) $ map rowToArray input
    rowToArray row = listArray (0, length row-1) $ map intToState row
    rows = length input
    -- This is a partial function, but it's only used for
    -- tests and an empty array isn't valid input
    cols = length (input !! 0)

stateToInt :: State -> Int
stateToInt Alive = 1
stateToInt Dead  = 0

intToState :: Int -> State
intToState 1 = Alive
intToState _ = Dead
