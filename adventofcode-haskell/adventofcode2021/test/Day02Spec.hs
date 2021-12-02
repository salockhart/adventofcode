module Day02Spec (spec) where

import Day02 (part1, part2)
import Test.Hspec

input =
  "forward 5\n\
  \down 5\n\
  \forward 8\n\
  \up 3\n\
  \down 8\n\
  \forward 2"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "150"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "900"
