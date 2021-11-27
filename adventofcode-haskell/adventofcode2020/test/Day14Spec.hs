module Day14Spec (spec) where

import Day14 (part1, part2)
import Test.Hspec

input1 =
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
  \mem[8] = 11\n\
  \mem[7] = 101\n\
  \mem[8] = 0"

input2 =
  "mask = 000000000000000000000000000000X1001X\n\
  \mem[42] = 100\n\
  \mask = 00000000000000000000000000000000X0XX\n\
  \mem[26] = 1"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 165 for " ++ input1) $ do
      part1 input1 `shouldBe` "165"

  describe "Part2" $ do
    it ("returns 208 for " ++ input2) $ do
      part2 input2 `shouldBe` "208"
