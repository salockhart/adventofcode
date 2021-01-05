module Day11Spec (spec) where

import Day11 (part1, part2)
import Test.Hspec

input = "abcdefgh"

input2 = "ghijklmn"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns abcdffaa for " ++ input) $ do
      part1 input `shouldBe` "\"abcdffaa\""
