module Day16Spec (spec) where

import Day16 (part1, part2)
import Test.Hspec

input =
  "class: 1-3 or 5-7\n\
  \row: 6-11 or 33-44\n\
  \seat: 13-40 or 45-50\n\
  \\n\
  \your ticket:\n\
  \7,1,14\n\
  \\n\
  \nearby tickets:\n\
  \7,3,47\n\
  \40,4,50\n\
  \55,2,20\n\
  \38,6,12"

input2 =
  "departure class: 0-1 or 4-19\n\
  \departure row: 0-5 or 8-19\n\
  \departure seat: 0-13 or 16-19\n\
  \\n\
  \your ticket:\n\
  \11,12,13\n\
  \\n\
  \nearby tickets:\n\
  \3,9,18\n\
  \15,1,5\n\
  \5,14,9"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 71 for " ++ input) $ do
      part1 input `shouldBe` "71"

  describe "Part2" $ do
    it ("returns 1716 for " ++ input2) $ do
      part2 input2 `shouldBe` "1716"
