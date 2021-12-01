module Day01 (main, part1, part2) where

import Control.Applicative.Combinators (count)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [Int]
parse = map read . lines

pair :: [b] -> [(b, b)]
pair xs = zip xs (drop 1 xs)

triple xs = zip3 xs (drop 1 xs) (drop 2 xs)

part1 :: String -> String
part1 =
  show
    . length
    . filter (uncurry (<))
    . pair
    . parse

part2 :: String -> String
part2 =
  show
    . length
    . filter (uncurry (<))
    . pair
    . map (\(a, b, c) -> a + b + c)
    . triple
    . parse
