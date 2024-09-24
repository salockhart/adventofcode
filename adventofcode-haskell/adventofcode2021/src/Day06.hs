{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day06 (main, part1, part2) where

import AOC.Data.String (splitOn)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

next (zeroes : rest) =
  let (zeroToFive, sixes : sevens : _) = splitAt 6 rest
   in zeroToFive ++ [sixes + zeroes, sevens, zeroes]

parse = foldl f (replicate 9 0) . map read . splitOn ","
  where
    f ns i =
      let (ns', n : ns'') = splitAt i ns
       in ns' ++ n + 1 : ns''

part1 :: String -> String
part1 =
  show
    . sum
    . (!! 80)
    . iterate next
    . parse

part2 :: String -> String
part2 =
  show
    . sum
    . (!! 256)
    . iterate next
    . parse
