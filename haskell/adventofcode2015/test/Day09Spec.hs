module Day09Spec (spec) where

import Day09 (part1, part2)
import Test.Hspec

input =
  "London to Dublin = 464\n\
  \London to Belfast = 518\n\
  \Dublin to Belfast = 141"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 605 for " ++ input) $ do
      part1 input `shouldBe` "605"

  describe "Part2" $ do
    it ("returns 982 for " ++ input) $ do
      part2 input `shouldBe` "982"
