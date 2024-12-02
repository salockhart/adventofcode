module Day02Spec (spec) where

import qualified Data.Text as T
import Day02 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "7 6 4 2 1\n\
  \1 2 7 8 9\n\
  \9 7 6 2 1\n\
  \1 3 2 4 5\n\
  \8 6 4 4 1\n\
  \1 3 6 7 9"

-- this input catches a naive scan
-- since 29->32 looks good, but 32 needs to be removed
input2 :: T.Text
input2 =
  "29 32 30 31 34 35 37"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 2)
    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 0)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 4)
    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 1)
