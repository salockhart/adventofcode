module Day03 (main, part1, part2) where

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

coord :: Int -> Int -> [String] -> Char
coord y x map = do
  let line = map !! y
  let x' = x `mod` length line
  line !! x'

toboggan :: Int -> Int -> [String] -> [Char]
toboggan right down map =
  [ coord i (i * right `div` down) map | i <- [0, down .. length map - 1]
  ]

countTrees :: [Char] -> Int
countTrees = length . filter (== '#')

part1 :: String -> Int
part1 = countTrees . toboggan 3 1 . lines

part2 :: String -> Int
part2 = product . map countTrees . toboggans . lines
  where
    toboggans lines =
      [ toboggan 1 1 lines,
        toboggan 3 1 lines,
        toboggan 5 1 lines,
        toboggan 7 1 lines,
        toboggan 1 2 lines
      ]
