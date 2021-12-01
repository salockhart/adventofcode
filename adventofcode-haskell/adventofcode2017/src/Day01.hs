module Day01 (main, part1, part2) where

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: [Char] -> [Int]
parse = map (\a -> read [a])

part1 :: String -> String
part1 =
  show
    . sum
    . map fst
    . filter (uncurry (==))
    . (\xs -> zip xs $ drop 1 $ cycle xs)
    . parse
    . head
    . lines

part2 :: String -> String
part2 =
  show
    . sum
    . map fst
    . filter (uncurry (==))
    . (\xs -> zip xs $ drop (quot (length xs) 2) $ cycle xs)
    . parse
    . head
    . lines
