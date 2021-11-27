module Day08Spec (spec) where

import Day08 (part1, part2)
import Test.Hspec

input =
  "\"\"\n\
  \\"abc\"\n\
  \\"aaa\\\"aaa\"\n\
  \\"\\x27\""

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 12 for " ++ input) $ do
      part1 input `shouldBe` "12"

  describe "Part2" $ do
    it ("returns 19 for " ++ input) $ do
      part2 input `shouldBe` "19"
