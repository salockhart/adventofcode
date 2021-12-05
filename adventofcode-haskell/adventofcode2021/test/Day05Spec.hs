module Day05Spec (spec) where

import Day05 (part1, part2)
import Test.Hspec

input =
  "0,9 -> 5,9\n\
  \8,0 -> 0,8\n\
  \9,4 -> 3,4\n\
  \2,2 -> 2,1\n\
  \7,0 -> 7,4\n\
  \6,4 -> 2,0\n\
  \0,9 -> 2,9\n\
  \3,4 -> 1,4\n\
  \0,0 -> 8,8\n\
  \5,5 -> 8,2"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "5"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "12"
