module Day12Spec (spec) where

import Day12 (part1, part2)
import Test.Hspec

input =
  "start-A\n\
  \start-b\n\
  \A-c\n\
  \A-b\n\
  \b-d\n\
  \A-end\n\
  \b-end"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "10"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "36"
