module Day01 (main, part1, part2) where

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [Int]
parse = map (\p -> if p == '(' then 1 else -1)

part1 :: String -> Int
part1 = sum . parse

part2 :: String -> Int
part2 = length . takeWhile (>= 0) . scanl (+) 0 . parse