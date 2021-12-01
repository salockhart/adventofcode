module Day01Spec (spec) where

import Day01 (part1, part2)
import Test.Hspec

input =
  "199\n\
  \200\n\
  \208\n\
  \210\n\
  \200\n\
  \207\n\
  \240\n\
  \269\n\
  \260\n\
  \263"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "7"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "5"
