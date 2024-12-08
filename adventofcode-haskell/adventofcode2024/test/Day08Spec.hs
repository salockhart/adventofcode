module Day08Spec (spec) where

import qualified Data.Text as T
import Day08 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "............\n\
  \........0...\n\
  \.....0......\n\
  \.......0....\n\
  \....0.......\n\
  \......A.....\n\
  \............\n\
  \............\n\
  \........A...\n\
  \.........A..\n\
  \............\n\
  \............"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 14)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 34)
