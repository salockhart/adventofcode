module Day09Spec (spec) where

import Day09 (part1, part2)
import Test.Hspec

input =
  "35\n\
  \20\n\
  \15\n\
  \25\n\
  \47\n\
  \40\n\
  \62\n\
  \55\n\
  \65\n\
  \95\n\
  \102\n\
  \117\n\
  \150\n\
  \182\n\
  \127\n\
  \219\n\
  \299\n\
  \277\n\
  \309\n\
  \576"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 127 for " ++ input) $ do
      part1 5 input `shouldBe` 127

  describe "Part2" $ do
    it ("returns 62 for " ++ input) $ do
      part2 5 input `shouldBe` 62
