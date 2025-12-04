module Day03 (main, part1, part2) where

import AOC (mkAoCMain)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2025 03 part1 part2

takeHighest :: (Ord a) => Int -> [a] -> [a]
takeHighest 0 _ = []
takeHighest n xs =
  let xs' = take (length xs - n + 1) xs
      maxX = maximum xs'
      rest = tail (dropWhile (/= maxX) xs)
   in maxX : takeHighest (n - 1) rest

solve :: Int -> T.Text -> Int
solve n =
  sum
    . map
      ( sum
          . zipWith (\i x -> x * 10 ^ i) [0 :: Int ..]
          . reverse
          . takeHighest n
          . map ((read :: String -> Int) . (: []))
      )
    . lines
    . T.unpack

part1 :: T.Text -> Int
part1 = solve 2

part2 :: T.Text -> Int
part2 = solve 12
