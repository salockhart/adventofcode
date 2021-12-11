module Day11Spec (spec) where

import Day11 (part1, part2)
import Test.Hspec

input =
  "5483143223\n\
  \2745854711\n\
  \5264556173\n\
  \6141336146\n\
  \6357385478\n\
  \4167524645\n\
  \2176841721\n\
  \6882881134\n\
  \4846848554\n\
  \5283751526"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "1656"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "195"
