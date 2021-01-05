module Day14Spec (spec) where

import Day14 (part1, part2)
import Test.Hspec

input =
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
  \Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 1120 for " ++ input) $ do
      part1 1000 input `shouldBe` "1120"

  describe "Part2" $ do
    it ("returns 689 for " ++ input) $ do
      part2 1000 input `shouldBe` "689"
