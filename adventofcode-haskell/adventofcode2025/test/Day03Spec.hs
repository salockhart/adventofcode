module Day03Spec (spec) where

import qualified Data.Text as T
import Day03 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "987654321111111\n\
  \811111111111119\n\
  \234234234234278\n\
  \818181911112111"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 357)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 3121910778619)
