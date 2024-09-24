module Day13 (main, part1, part2) where

import AOC.Data.List (chunks, maximumOn)
import Data.List (foldl', nub, permutations)
import qualified Data.Map as Map
import Text.Regex.PCRE ((=~))

type Relationship = ((String, String), Int)

type RGraph = Map.Map (String, String) Int

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> Relationship
parse str = do
  let (_, _, _, a : gl : amt : b : _) = str =~ "([a-zA-Z]+) would (gain|lose) ([0-9]+) happiness units by sitting next to ([a-zA-Z]+)." :: (String, String, String, [String])
  let modifier = if gl == "gain" then 1 else -1
  ((a, b), modifier * read amt)

buildGraph :: [Relationship] -> RGraph
buildGraph = foldl' (\m ((a, b), i) -> Map.insertWith (+) (a, b) i m) Map.empty

actors :: RGraph -> [String]
actors = nub . map fst . Map.keys

solve :: RGraph -> ([String], Int)
solve rs = do
  let possibles = permutations $ actors rs
  let weights = map (sum . map (\[a, b] -> rs Map.! (a, b) + rs Map.! (b, a)) . chunks 2 . (\ps -> take (length ps + 1) $ cycle ps)) possibles
  let results = zip possibles weights
  let best = maximumOn snd results
  best

addSelf :: RGraph -> RGraph
addSelf rs = do
  let as = actors rs
  foldl' (\m a -> Map.insert ("Me", a) 0 $ Map.insert (a, "Me") 0 m) rs as

part1 :: String -> String
part1 = show . snd . solve . buildGraph . map parse . lines

part2 :: String -> String
part2 = show . snd . solve . addSelf . buildGraph . map parse . lines
