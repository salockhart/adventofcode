module Day19Spec (spec) where

import Day19 (part1, part2)
import Test.Hspec

input1 =
  "e => H\n\
  \e => O\n\
  \H => HO\n\
  \H => OH\n\
  \O => HH\n\
  \\n\
  \HOH"

input2 =
  "e => H\n\
  \e => O\n\
  \H => HO\n\
  \H => OH\n\
  \O => HH\n\
  \\n\
  \HOHOHO"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 4 for " ++ input1) $ do
      part1 input1 `shouldBe` "4"
    it ("returns 7 for " ++ input2) $ do
      part1 input2 `shouldBe` "7"
