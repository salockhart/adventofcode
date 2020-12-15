module Day12Spec (spec) where

import Day12 (part1, part2)
import Test.Hspec

input =
  "F10\n\
  \N3\n\
  \F7\n\
  \R90\n\
  \F11"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 25 for " ++ input) $ do
      part1 input `shouldBe` "25"

  describe "Part2" $ do
    it ("returns 286 for " ++ input) $ do
      part2 input `shouldBe` "286"
