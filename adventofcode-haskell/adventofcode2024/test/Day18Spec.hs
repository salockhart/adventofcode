module Day18Spec (spec) where

import qualified Data.Text as T
import Day18 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "5,4\n\
  \4,2\n\
  \4,5\n\
  \3,0\n\
  \2,1\n\
  \6,3\n\
  \2,4\n\
  \1,5\n\
  \0,6\n\
  \3,3\n\
  \2,6\n\
  \5,1\n\
  \1,2\n\
  \5,5\n\
  \2,5\n\
  \6,5\n\
  \1,4\n\
  \0,4\n\
  \6,4\n\
  \1,1\n\
  \6,1\n\
  \1,0\n\
  \0,5\n\
  \1,6\n\
  \2,0"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 12 input `shouldSatisfy` (== 22)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 12 input `shouldSatisfy` (== "6,1")
