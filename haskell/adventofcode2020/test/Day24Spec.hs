module Day24Spec (spec) where

import Day24 (part1, part2)
import Test.Hspec

input =
  "sesenwnenenewseeswwswswwnenewsewsw\n\
  \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
  \seswneswswsenwwnwse\n\
  \nwnwneseeswswnenewneswwnewseswneseene\n\
  \swweswneswnenwsewnwneneseenw\n\
  \eesenwseswswnenwswnwnwsewwnwsene\n\
  \sewnenenenesenwsewnenwwwse\n\
  \wenwwweseeeweswwwnwwe\n\
  \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
  \neeswseenwwswnwswswnw\n\
  \nenwswwsewswnenenewsenwsenwnesesenew\n\
  \enewnwewneswsewnwswenweswnenwsenwsw\n\
  \sweneswneswneneenwnewenewwneswswnese\n\
  \swwesenesewenwneswnwwneseswwne\n\
  \enesenwswwswneneswsenwnewswseenwsese\n\
  \wnwnesenesenenwwnenwsewesewsesesew\n\
  \nenewswnwewswnenesenwnesewesw\n\
  \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
  \neswnwewnwnwseenwseesewsenwsweewe\n\
  \wseweeenwnesenwwwswnew"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 10 for " ++ input) $ do
      part1 input `shouldBe` "10"

-- describe "Part2" $ do
--   it ("returns 2208 for " ++ input) $ do
--     part2 input `shouldBe` "2208"
