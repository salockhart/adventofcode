module Day14Spec (spec) where

import qualified Data.Text as T
import Day14 (part1, part2)
import Test.Hspec

bounds = (11, 7)

input :: T.Text
input =
  "p=0,4 v=3,-3\n\
  \p=6,3 v=-1,-3\n\
  \p=10,3 v=-1,2\n\
  \p=2,0 v=2,-1\n\
  \p=0,0 v=1,3\n\
  \p=3,0 v=-2,-2\n\
  \p=7,6 v=-1,-3\n\
  \p=3,0 v=-1,-2\n\
  \p=9,3 v=2,3\n\
  \p=7,3 v=-1,2\n\
  \p=2,4 v=2,-3\n\
  \p=9,5 v=-3,-3"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 bounds input `shouldSatisfy` (== 12)
