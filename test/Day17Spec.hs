module Day17Spec (spec) where

import Day17 (part1, part2)
import Test.Hspec

input =
  ".#.\n\
  \..#\n\
  \###"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 112 for " ++ input) $ do
      part1 input `shouldBe` "112"

-- describe "Part2" $ do
--   it ("returns 848 for " ++ input) $ do
--     part2 input `shouldBe` "848"
