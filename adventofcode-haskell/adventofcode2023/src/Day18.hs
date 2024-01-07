module Day18 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (picksTheorem', shoelaceTheorem)
import AOC.Data.List (mapWithPrevious, unsnoc)
import AOC.Data.String (readHex)
import AOC.Data.Tuple (take2Of3, trdOf3)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Debug.Trace (trace)

main :: IO ()
main = mkAoCMain 2023 18 part1 part2

parse :: [[String]] -> [(Int, Int, Int)]
parse =
  mapWithPrevious (\(ax, ay, ac) (bx, by, bc) -> (ax + bx, ay + by, ac + bc))
    . map parse'
  where
    parse' [s, x] =
      let x' = read x
       in case s of
            "U" -> (0, -x', x')
            "D" -> (0, x', x')
            "L" -> (-x', 0, x')
            "R" -> (x', 0, x')
            _ -> error "can't parse"
    parse' _ = error "can't parse"

numToDirection :: Char -> String
numToDirection '0' = "R"
numToDirection '1' = "D"
numToDirection '2' = "L"
numToDirection '3' = "U"
numToDirection x = error ("invalid number (" ++ show x ++ "), no direction mapping")

solve :: [(Int, Int, Int)] -> Int
solve xs = do
  let circumference = trdOf3 (last xs)
  let points = map take2Of3 xs
  let area = shoelaceTheorem points
  let innerPoints = picksTheorem' area circumference
  trace ("circumference=" ++ show circumference ++ "; innerPoints=" ++ show innerPoints) (innerPoints + circumference)

part1 :: T.Text -> Int
part1 =
  solve
    . parse
    . map
      ( take 2
          . map T.unpack
          . T.splitOn " "
      )
    . T.lines

part2 :: T.Text -> Int
part2 =
  solve
    . parse
    . map
      ( (\(a, b) -> [b, show a])
          . bimap readHex numToDirection
          . fromJust
          . unsnoc
          . drop 2
          . init
          . last
          . map T.unpack
          . T.splitOn " "
      )
    . T.lines
