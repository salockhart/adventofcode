module Day2Spec (spec) where

import Day2 (part1, part2)
import Test.Hspec

input = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 1 for\n" ++ input) $ do
      part1 input `shouldBe` 2

  describe "Part2" $ do
    it ("returns 1 for\n" ++ input) $ do
      part2 input `shouldBe` 1
