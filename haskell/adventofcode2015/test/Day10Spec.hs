module Day10Spec (spec) where

import Day10 (part1, part2)
import Test.Hspec

input = "1"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 6 for " ++ input) $ do
      part1 5 input `shouldBe` "6"
