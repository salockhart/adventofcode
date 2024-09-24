{-# LANGUAGE TupleSections #-}

module Day19 (main, part1, part2) where

import AOC.Data.String (splitOn)
import Data.Char (isLower)
import Data.List (groupBy, nub)

type Molecules = [String]

type Production = (Molecules, Molecules)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: [String] -> ([Production], Molecules)
parse [ts, m] =
  ( map ((\[a, b] -> (groupMolecules a, groupMolecules b)) . splitOn " => ") $ lines ts,
    groupMolecules $ head $ lines m
  )

groupMolecules :: String -> Molecules
groupMolecules = groupBy (\_ b -> isLower b)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n ins as = take n as ++ [ins] ++ drop (n + 1) as

applyProductions :: ([Production], Molecules) -> [String]
applyProductions (m, els) =
  nub $
    map (concat . \(i, (_, to)) -> replaceAt i (concat to) els) $
      concatMap
        (\i -> map (i,) $ filter ((== [els !! i]) . fst) m)
        [0 .. length els - 1]

part1 :: String -> String
part1 = show . length . applyProductions . parse . splitOn "\n\n"

part2 :: String -> String
part2 = show . solve . snd . parse . splitOn "\n\n"
  where
    -- https://old.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4h7ji/
    solve :: Molecules -> Int
    solve ms = do
      let numMolecules = length ms
      let numRn = length $ filter (== "Rn") ms
      let numAr = length $ filter (== "Ar") ms
      let numY = length $ filter (== "Y") ms
      numMolecules - numRn - numAr - 2 * numY - 1
