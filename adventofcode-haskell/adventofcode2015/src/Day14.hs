{-# LANGUAGE TupleSections #-}

module Day14 (main, part1, part2) where

import AOC.Data.List (maximumOn)
import qualified Data.Bifunctor
import Data.List (group, scanl', sort, transpose)
import Text.Regex.PCRE ((=~))

type Reindeer = (String, [Int])

main :: IO ()
main = interact (show . \input -> (part1 2503 input, part2 2503 input))

parse :: String -> Reindeer
parse str = do
  let (_, _, _, name : speed : time : rest : _) = str =~ "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds." :: (String, String, String, [String])
  let speed' = read speed :: Int
  let time' = read time :: Int
  let rest' = read rest :: Int
  let flying = replicate time' speed'
  let resting = replicate rest' 0
  (name, flying ++ resting)

solve :: Int -> [Reindeer] -> [(String, Int)]
solve n = map (Data.Bifunctor.second (sum . take n . cycle))

solve' :: Int -> [Reindeer] -> [(String, Int)]
solve' n =
  map (\g -> (head g, length g - 1))
    . group
    . sort
    . concatMap (map fst . filterLeaders)
    . transpose
    . map
      ( (\(a, bs) -> map (a,) bs)
          . Data.Bifunctor.second (scanl' (+) 0 . take n . cycle)
      )
  where
    filterLeaders rs = do
      let lead = maximum $ map snd rs
      filter ((== lead) . snd) rs

part1 :: Int -> String -> String
part1 n = show . snd . maximumOn snd . solve n . map parse . lines

part2 :: Int -> String -> String
part2 n = show . snd . maximumOn snd . solve' n . map parse . lines
