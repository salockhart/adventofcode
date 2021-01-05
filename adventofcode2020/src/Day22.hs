module Day22 (main, part1, part2) where

import AOC (splitOn)

type Game = ([Int], [Int])

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)

parse :: String -> Game
parse = toTuple . map (map read . tail . lines) . splitOn "\n\n"

play :: Game -> ([Int], [Int])
play ([], bs) = ([], bs)
play (as, []) = (as, [])
play (a : as, b : bs)
  | a > b = play (as ++ [a, b], bs)
  | otherwise = play (as, bs ++ [b, a])

play' :: [Game] -> Game -> ([Int], [Int])
play' _ ([], bs) = ([], bs)
play' _ (as, []) = (as, [])
play' history game@(a : as, b : bs)
  | game `elem` history = (fst game, [])
  | a <= length as && b <= length bs =
    case play' [] (take a as, take b bs) of
      (_, []) -> play' (game : history) (as ++ [a, b], bs)
      ([], _) -> play' (game : history) (as, bs ++ [b, a])
  | a > b = play' (game : history) (as ++ [a, b], bs)
  | otherwise = play' (game : history) (as, bs ++ [b, a])

score :: ([Int], [Int]) -> Int
score ([], bs) = sum $ zipWith (*) [1 ..] (reverse bs)
score (as, []) = sum $ zipWith (*) [1 ..] (reverse as)

part1 :: String -> String
part1 = show . score . play . parse

part2 :: String -> String
part2 = show . score . play' [] . parse
