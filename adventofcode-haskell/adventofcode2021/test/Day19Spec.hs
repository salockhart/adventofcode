module Day19Spec (spec) where

import Day19 (part1, part2)
import Test.Hspec

input = ""

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "1"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "1"
