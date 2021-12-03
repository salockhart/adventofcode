module Day03Spec (spec) where

import Day03 (part1, part2)
import Test.Hspec

input =
  "00100\n\
  \11110\n\
  \10110\n\
  \10111\n\
  \10101\n\
  \01111\n\
  \00111\n\
  \11100\n\
  \10000\n\
  \11001\n\
  \00010\n\
  \01010"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "198"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "230"
