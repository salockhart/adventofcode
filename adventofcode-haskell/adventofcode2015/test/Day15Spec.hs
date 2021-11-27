module Day15Spec (spec) where

import Day15 (part1, part2)
import Test.Hspec

input =
  "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\n\
  \Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 62842880 for " ++ input) $ do
      part1 input `shouldBe` "62842880"

  describe "Part2" $ do
    it ("returns 57600000 for " ++ input) $ do
      part2 input `shouldBe` "57600000"
