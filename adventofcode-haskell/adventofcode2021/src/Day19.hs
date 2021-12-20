module Day19 (main, part1, part2) where

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

part1 :: String -> String
part1 = show . const 1

part2 :: String -> String
part2 = show . const 1
