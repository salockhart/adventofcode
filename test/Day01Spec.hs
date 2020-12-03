module Day01Spec (spec) where

import Day01 (part1, part2)
import Test.Hspec

input = "1721\n979\n366\n299\n675\n1456"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 514579 for\n" ++ input) $ do
      part1 input `shouldBe` 514579

  describe "Part2" $ do
    it ("returns 241861950 for\n" ++ input) $ do
      part2 input `shouldBe` 241861950
