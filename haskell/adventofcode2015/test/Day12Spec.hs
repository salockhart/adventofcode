module Day12Spec (spec) where

import Day12 (part1, part2)
import Test.Hspec

input1 = "[1,2,3]"

input2 = "{\"a\":2,\"b\":4}"

input3 = "[[[3]]]"

input4 = "{\"a\":{\"b\":4},\"c\":-1}"

input5 = "{\"a\":[-1,1]}"

input6 = "[-1,{\"a\":1}]"

input7 = "[]"

input8 = "{}"

input9 = "[1,{\"c\":\"red\",\"b\":2},3]"

input10 = "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"

input11 = "[1,\"red\",5]"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 6 for " ++ input1) $ do
      part1 input1 `shouldBe` "6"
    it ("returns 6 for " ++ input2) $ do
      part1 input2 `shouldBe` "6"

    it ("returns 3 for " ++ input3) $ do
      part1 input3 `shouldBe` "3"
    it ("returns 3 for " ++ input4) $ do
      part1 input4 `shouldBe` "3"

    it ("returns 0 for " ++ input5) $ do
      part1 input5 `shouldBe` "0"
    it ("returns 0 for " ++ input6) $ do
      part1 input6 `shouldBe` "0"

    it ("returns 0 for " ++ input7) $ do
      part1 input7 `shouldBe` "0"
    it ("returns 0 for " ++ input8) $ do
      part1 input8 `shouldBe` "0"

  describe "Part2" $ do
    it ("returns 4 for " ++ input9) $ do
      part2 input9 `shouldBe` "4"
    it ("returns 0 for " ++ input10) $ do
      part2 input10 `shouldBe` "0"
    it ("returns 6 for " ++ input11) $ do
      part2 input11 `shouldBe` "6"
