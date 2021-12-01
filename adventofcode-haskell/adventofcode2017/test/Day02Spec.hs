module Day02Spec (spec) where

import Day02 (part1, part2)
import Test.Hspec

input =
  "5 1 9 5\n\
  \7 5 3\n\
  \2 4 6 8"

input2 =
  "5 9 2 8\n\
  \9 4 7 3\n\
  \3 8 6 5"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "18"

  describe "Part2" $ do
    it input2 $ do
      part2 input2 `shouldBe` "9"
