module Day11Spec (spec) where

import qualified Data.Text as T
import Day11 (part1, part2)
import Test.Hspec

input :: T.Text
input = "125 17"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 55312)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 65601038650482)
