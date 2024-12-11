module Day06 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Control.Monad.State (anyM)
import AOC.CoordMap (Coord, CoordMap, east, north, readCoordMap, south, west)
import Control.Monad.State (State, evalState, get, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Direction = N | E | S | W deriving (Show, Ord, Eq)

data Ray = Ray Direction [Coord] deriving (Show)

main :: IO ()
main = mkAoCMain 2024 06 part1 part2

parse :: T.Text -> CoordMap Char
parse = readCoordMap . lines . T.unpack

turn90 :: Direction -> Direction
turn90 N = E
turn90 E = S
turn90 S = W
turn90 W = N

step :: CoordMap b -> Coord -> Direction -> Maybe (Coord, b)
step cm c N = north c cm
step cm c E = east c cm
step cm c S = south c cm
step cm c W = west c cm

walk :: CoordMap Char -> Coord -> Direction -> [(Coord, Direction)]
walk cm c d = case step cm c d of
  Just (_, '#') -> walk cm c (turn90 d)
  Just (c', _) -> (c, d) : walk cm c' d
  Nothing -> [(c, d)]

isDuplicate :: (Ord a) => a -> State (S.Set a) Bool
isDuplicate a = do
  seen <- get
  let dupe = a `S.member` seen
  modify (S.insert a)
  return dupe

anyDuplicate :: (Ord a) => [a] -> Bool
anyDuplicate as = evalState (anyDuplicate' as) S.empty
  where
    anyDuplicate' :: (Ord a) => [a] -> State (S.Set a) Bool
    anyDuplicate' = anyM isDuplicate

solve :: M.Map Coord Char -> [(Coord, Direction)]
solve cm = do
  let start = head $ M.keys $ M.filter (== '^') cm
  let cm' = M.map (\c -> if c == '^' then '.' else c) cm
  walk cm' start N

part1 :: T.Text -> Int
part1 = length . S.fromList . map fst . solve . parse

part2 :: T.Text -> Int
part2 = length . filter id . map (anyDuplicate . solve) . possibles . parse
  where
    possibles cm = do
      let options = M.keys $ M.filter (== '.') cm
      map (\c -> M.union (M.fromList [(c, '#')]) cm) options
