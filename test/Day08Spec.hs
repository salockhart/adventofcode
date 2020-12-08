module Day08Spec (spec) where

import Day08 (part1, part2)
import Test.Hspec

input =
  "nop +0\n\
  \acc +1\n\
  \jmp +4\n\
  \acc +3\n\
  \jmp -3\n\
  \acc -99\n\
  \acc +1\n\
  \jmp -4\n\
  \acc +6"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 5 for " ++ input) $ do
      part1 input `shouldBe` 5

  describe "Part2" $ do
    it ("returns 8 for " ++ input) $ do
      part2 input `shouldBe` 8
