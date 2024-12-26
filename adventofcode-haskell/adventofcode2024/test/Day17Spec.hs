module Day17Spec (spec) where

import qualified Data.Text as T
import Day17 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "Register A: 729\n\
  \Register B: 0\n\
  \Register C: 0\n\
  \\n\
  \Program: 0,1,5,4,3,0"

input2 :: T.Text
input2 =
  "Register A: 2024\n\
  \Register B: 0\n\
  \Register C: 0\n\
  \\n\
  \Program: 0,3,5,4,3,0"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== "4,6,3,5,6,3,5,2,1,0")

  describe "Part2" $ do
    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 117440)
