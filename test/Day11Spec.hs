module Day11Spec (spec) where

import Day11 (part1, part2)
import Test.Hspec

input =
  "L.LL.LL.LL\n\
  \LLLLLLL.LL\n\
  \L.L.L..L..\n\
  \LLLL.LL.LL\n\
  \L.LL.LL.LL\n\
  \L.LLLLL.LL\n\
  \..L.L.....\n\
  \LLLLLLLLLL\n\
  \L.LLLLLL.L\n\
  \L.LLLLL.LL"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 37 for " ++ input) $ do
      part1 input `shouldBe` "37"

  describe "Part2" $ do
    it ("returns 26 for " ++ input) $ do
      part2 input `shouldBe` "26"
