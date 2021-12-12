module Day12 (main, part1, part2) where

-- with help from https://gitlab.com/sakisan/adventofcode/-/blob/2021/Haskell/Day12.hs

import AOC (dbg, splitOn)
import Data.Char (isLower)
import Data.Map (Map)
import qualified Data.Map as Map

type CaveMap = Map String [String]

type Pos = (String, Map String Int)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> CaveMap
parse =
  Map.fromListWith (++)
    . concatMap ((\(a : b : _) -> [(a, [b]), (b, [a])]) . splitOn "-")
    . lines

isSmallCave :: String -> Bool
isSmallCave = all isLower

markVisited :: Num a => Map String a -> String -> Map String a
markVisited visited node =
  if isSmallCave node
    then Map.insertWith (+) node 1 visited
    else visited

neighbors :: CaveMap -> Pos -> [Pos]
neighbors graph (current, visited) =
  [ (node, markVisited visited node)
    | node <- graph Map.! current,
      node /= "start",
      node `Map.notMember` visited
  ]

neighbors' :: CaveMap -> Pos -> [Pos]
neighbors' graph (current, visited) =
  [ (node, markVisited visited node)
    | node <- graph Map.! current,
      node /= "start",
      goTwice || node `Map.notMember` visited
  ]
  where
    goTwice = all (<= 1) $ Map.elems visited

search :: (Pos -> [Pos]) -> String -> [Pos] -> Int -> Int
search neighbors target [] c = c
search neighbors target (pos@(node, _) : paths) c
  | node == target = search neighbors target paths (c + 1)
  | otherwise = search neighbors target queue c
  where
    queue = neighbors pos ++ paths

part1 :: String -> String
part1 =
  show
    . solve
    . parse
  where
    solve g =
      search
        (neighbors g)
        "end"
        [("start", Map.empty)]
        0

part2 :: String -> String
part2 =
  show
    . solve
    . parse
  where
    solve g =
      search
        (neighbors' g)
        "end"
        [("start", Map.empty)]
        0
