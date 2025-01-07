module Day21Spec (spec) where

import qualified Data.Text as T
import Day21 (part1)
import Test.Hspec

input :: T.Text
input =
  "029A\n\
  \980A\n\
  \179A\n\
  \456A\n\
  \379A"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 126384)
