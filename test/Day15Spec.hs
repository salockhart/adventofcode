module Day15Spec (spec) where

import Day15 (part1, part2)
import Test.Hspec

input1 = "0,3,6"

input2 = "1,3,2"

input3 = "2,1,3"

input4 = "1,2,3"

input5 = "2,3,1"

input6 = "3,2,1"

input7 = "3,1,2"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 436 for " ++ input1) $ do
      part1 input1 `shouldBe` "436"
    it ("returns 1 for " ++ input2) $ do
      part1 input2 `shouldBe` "1"
    it ("returns 10 for " ++ input3) $ do
      part1 input3 `shouldBe` "10"
    it ("returns 27 for " ++ input4) $ do
      part1 input4 `shouldBe` "27"
    it ("returns 78 for " ++ input5) $ do
      part1 input5 `shouldBe` "78"
    it ("returns 438 for " ++ input6) $ do
      part1 input6 `shouldBe` "438"
    it ("returns 1836 for " ++ input7) $ do
      part1 input7 `shouldBe` "1836"

-- describe "Part2" $ do
-- it ("returns 175594 for " ++ input1) $ do
--   part2 input1 `shouldBe` "175594"
--   it ("returns 2578 for " ++ input2) $ do
--     part2 input2 `shouldBe` "2578"
--   it ("returns 3544142 for " ++ input3) $ do
--     part2 input3 `shouldBe` "3544142"
--   it ("returns 261214 for " ++ input4) $ do
--     part2 input4 `shouldBe` "261214"
--   it ("returns 6895259 for " ++ input5) $ do
--     part2 input5 `shouldBe` "6895259"
--   it ("returns 18 for " ++ input6) $ do
--     part2 input6 `shouldBe` "18"
--   it ("returns 362 for " ++ input7) $ do
--     part2 input7 `shouldBe` "362"
