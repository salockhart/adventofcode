module Day17Spec (spec) where

import Day17 (part1, part2)
import Test.Hspec

input = ""

spec :: Spec
spec = do
  describe "Part1" $ do
    it "returns 4 for 25L from 20, 15, 10, 5, and 5" $ do
      part1 25 "20\n15\n10\n5\n5" `shouldBe` "4"

  describe "Part2" $ do
    it "returns 3 for 25L from 20, 15, 10, 5, and 5" $ do
      part2 25 "20\n15\n10\n5\n5" `shouldBe` "3"
