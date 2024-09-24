module Day21 (main, part1, part2) where

import AOC.Data.String (splitOn)
import Data.List (find, foldl', intercalate, sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set

type IngredientMap = Map.Map String (Set.Set String)

type IngredientMap' = Map.Map String String

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> (Set.Set String, [String])
parse str = do
  let [ingredients, allergens] = splitOn "(contains" str
  (Set.fromList (words ingredients), map init $ words allergens)

normalise :: IngredientMap -> IngredientMap'
normalise map = do
  let solved = find (\(_, l) -> length l == 1) $ Map.toList map
  case solved of
    Just (allergen, set) -> Map.insert allergen (Set.elemAt 0 set) $ normalise (Map.map (Set.filter (`notElem` set)) (Map.delete allergen map))
    Nothing -> Map.empty

solve :: IngredientMap -> [(Set.Set String, [String])] -> IngredientMap'
solve map [] = normalise map
solve map ((ingredients, allergens) : rest) = do
  let map' = foldl' insertIngredients map allergens
  solve map' rest
  where
    insertIngredients m a = Map.insertWithKey combineSets a ingredients m
    combineSets _ new old = Set.intersection new old

part1 :: String -> String
part1 = show . solve' . map parse . lines
  where
    solve' pairs = do
      let m = solve Map.empty pairs
      let ingredients = concatMap (Set.toList . fst) pairs
      length $ filter (\i -> i `notElem` Map.elems m) ingredients

part2 :: String -> String
part2 = show . solve' . map parse . lines
  where
    solve' pairs = do
      let m = solve Map.empty pairs
      let m' = sortOn fst $ Map.toList m
      intercalate "," $ map snd m'
