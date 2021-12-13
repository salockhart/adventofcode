module Day03Spec (spec) where

import Day03 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    -- it "1" $ do
    --   part1 "1" `shouldBe` "0"
    -- it "12" $ do
    --   part1 "12" `shouldBe` "3"
    -- it "23" $ do
    --   part1 "23" `shouldBe` "2"
    it "1024" $ do
      part1 "1024" `shouldBe` "31"
  -- it "9" $ do
  --   part1 "9" `shouldBe` "2"

  describe "Part2" $ do
    it "1" $ do
      part2 "1" `shouldBe` "1"
