module Day23Spec (spec) where

import Day23 (part1, part2)
import Test.Hspec

input = "389125467"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 67384529 for " ++ input) $ do
      part1 input `shouldBe` "67384529"

-- describe "Part2" $ do
--   it ("returns 149245887792 for " ++ input) $ do
--     part2 input `shouldBe` "149245887792"
