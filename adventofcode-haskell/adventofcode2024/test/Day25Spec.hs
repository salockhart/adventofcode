module Day25Spec (spec) where

import qualified Data.Text as T
import Day25 (part1)
import Test.Hspec

input :: T.Text
input =
  "#####\n\
  \.####\n\
  \.####\n\
  \.####\n\
  \.#.#.\n\
  \.#...\n\
  \.....\n\
  \\n\
  \#####\n\
  \##.##\n\
  \.#.##\n\
  \...##\n\
  \...#.\n\
  \...#.\n\
  \.....\n\
  \\n\
  \.....\n\
  \#....\n\
  \#....\n\
  \#...#\n\
  \#.#.#\n\
  \#.###\n\
  \#####\n\
  \\n\
  \.....\n\
  \.....\n\
  \#.#..\n\
  \###..\n\
  \###.#\n\
  \###.#\n\
  \#####\n\
  \\n\
  \.....\n\
  \.....\n\
  \.....\n\
  \#....\n\
  \#.#..\n\
  \#.#.#\n\
  \#####"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 3)
