module Main where

import Test.Hspec
import Simulation
import Data.Array
import Test.QuickCheck

main :: IO ()
main = hspec spec

instance Arbitrary Coord where
  arbitrary = Coord <$> r <*> r
    where r = arbitrary

spec :: Spec
spec = do
  let testBoard = toBoard [
                            [1,0,1],
                            [1,1,0],
                            [1,0,0],
                            [0,0,1]
                          ]
  describe "stepCell" $ do
    it "should revive dead cells if num_neighbors is 3" $
      stepCell Dead 3 `shouldBe` Alive
    it "should not revive dead cells ∀ num_neighbors in [0,3) ∪ (3,8]" $
      mapM_ (\x -> stepCell Dead x `shouldBe` Dead) ([0..2] ++ [4..8])
    it "should kill live cells ∀ num_neighbors in [0,1] ∪ [4,8]" $
      mapM_ (\x -> stepCell Alive x `shouldBe` Dead) ([0..1] ++ [4..8])
    it "should keep cells alive ∀ num_neighbors in [2,3]" $
      mapM_ (\x -> stepCell Alive x `shouldBe` Alive) [2,3]

  describe "<+>" $ do
    it "should add two cords together" $
      Coord 1 2 <+> Coord 4 5 `shouldBe` Coord 5 7
    it "should satisfy the commutative property" $ do
      let prop_commutative x y = x <+> y == y <+> x
      quickCheck prop_commutative
    it "should satisfy the associative property" $ do
      let prop_associative x y z = (x <+> y) <+> z == x <+> (y <+> z)
      quickCheck prop_associative
    it "should satisfy the additive identity property" $ do
      let prop_identity x = (x <+> Coord 0 0) == x
      quickCheck prop_identity

  describe "numNeighbors" $ do
    it "should find the # of neighbors around a cell" $
      neighbors testBoard (Coord 1 1) `shouldBe` 4
    it "should handle corners" $ do
      neighbors testBoard (Coord 0 0) `shouldBe` 2
      neighbors testBoard (Coord 0 2) `shouldBe` 1
      neighbors testBoard (Coord 3 0) `shouldBe` 1
      neighbors testBoard (Coord 3 2) `shouldBe` 0
    it "should handle edges" $ do
      neighbors testBoard (Coord 1 0) `shouldBe` 3
      neighbors testBoard (Coord 0 1) `shouldBe` 4

  describe "validCoord" $ do
    it "should work for valid coords" $ do
      let validCoords = uncurry Coord <$> [(1,2), (0,0), (3,0), (3,2)]
      mapM_ (shouldBe True . validCoord testBoard) validCoords
    it "should work for invalid coords" $ do
      let validCoords = uncurry Coord <$> [(-1,2), (0,5), (3,-3), (5,0)]
      mapM_ (shouldBe False . validCoord testBoard) validCoords

  describe "state" $
    it "should work" $ do
      state testBoard (Coord 1 1) `shouldBe` Alive
      state testBoard (Coord 0 1) `shouldBe` Dead
      state testBoard (Coord 2 2) `shouldBe` Dead
      state testBoard (Coord 3 2) `shouldBe` Alive

  describe "nextState" $
    it "should work" $ do
      nextState testBoard (Coord 1 1) `shouldBe` Dead
      nextState testBoard (Coord 3 2) `shouldBe` Dead
      nextState testBoard (Coord 0 0) `shouldBe` Alive

  describeStep

describeStep :: Spec
describeStep =
  describe "step" $ do
    it "should handle a very simple board" $ do
      let start = [ 
                    [0, 0, 1],
                    [1, 1, 1],
                    [0, 0, 1]
                  ]
      let expected_end = [
                    [0, 0, 0],
                    [0, 1, 1],
                    [0, 0, 0]
                  ]
      shouldBe True $ 
        test_game start 2 expected_end
                  
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
      let two_steps = toTestFormat <$> (take 3 . iterate step) board
      two_steps `shouldBe` [zeroth_step, first_step, second_step]


test_game :: [[Int]] -> Int -> [[Int]] -> Bool
test_game input n expected = toTestFormat (iterate step board !! n) == expected
  where
  board = toBoard input

-- Everything below is glue to convert to and from the test
-- format (which is easier to type and see visually), and
-- the actual type of the board

toTestFormat :: Board -> [[Int]]
toTestFormat board = fmap stateToInt <$> elems (fmap elems (bGrid board))

toBoard :: [[Int]] -> Board
toBoard input = Board {bGrid = grid, bHeight = height, bWidth = width}
  where
  grid = listArray (0, length input-1) $ map rowToArray input
  rowToArray row = listArray (0, length row-1) $ map intToState row
  height = length input
  -- This is a partial function, but it's only used for
  -- tests and an empty array isn't valid input
  width = length . head $ input

stateToInt :: State -> Int
stateToInt Alive = 1
stateToInt Dead  = 0

intToState :: Int -> State
intToState 1 = Alive
intToState _ = Dead
