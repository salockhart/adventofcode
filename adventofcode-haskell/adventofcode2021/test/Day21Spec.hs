module Day21Spec (spec) where

import Day21 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

input =
  "Player 1 starting position: 4\n\
  \Player 2 starting position: 8"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "739785"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "1"
