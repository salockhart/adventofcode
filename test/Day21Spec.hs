module Day21Spec (spec) where

import Day21 (part1, part2)
import Test.Hspec

input =
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
  \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
  \sqjhc fvjkl (contains soy)\n\
  \sqjhc mxmxvkd sbzzf (contains fish)"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 5 for " ++ input) $ do
      part1 input `shouldBe` "5"

  describe "Part2" $ do
    it ("returns mxmxvkd,sqjhc,fvjkl for " ++ input) $ do
      part2 input `shouldBe` "\"mxmxvkd,sqjhc,fvjkl\""
