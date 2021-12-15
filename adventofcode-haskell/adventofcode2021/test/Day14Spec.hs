module Day14Spec (spec) where

import Day14 (part1, part2)
import Test.Hspec

input =
  "NNCB\n\
  \\n\
  \CH -> B\n\
  \HH -> N\n\
  \CB -> H\n\
  \NH -> C\n\
  \HB -> C\n\
  \HC -> B\n\
  \HN -> C\n\
  \NN -> C\n\
  \BH -> H\n\
  \NC -> B\n\
  \NB -> B\n\
  \BN -> B\n\
  \BB -> N\n\
  \BC -> B\n\
  \CC -> N\n\
  \CN -> C"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "1588"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "2188189693529"
