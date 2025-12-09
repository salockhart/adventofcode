module Day08 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.String (splitOn)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (findIndex, nub, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2025 08 (part1 1000) part2

straightLineDistance :: [Int] -> [Int] -> Double
straightLineDistance as bs = sqrt $ sum $ zipWith (\a b -> (fromIntegral a - fromIntegral b) ^ 2) as bs

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

parse :: T.Text -> [([Int], [Int])]
parse =
  ( let fn = uncurry straightLineDistance
     in sortBy (\a b -> compare (fn a) (fn b))
  )
    . pairs
    . map (map (read :: String -> Int) . splitOn ",")
    . lines
    . T.unpack

connectCircuits :: (Ord b) => [(b, b)] -> ([S.Set b], Maybe (b, b))
connectCircuits ps = connectCircuits' initialCircuits ps
  where
    initialCircuits = nub $ concatMap (\(a, b) -> [S.singleton a, S.singleton b]) ps
    connectCircuits' circuits [] = (circuits, Nothing) -- part 1
    connectCircuits' circuits ((a, b) : xs) = do
      let ai = fromJust $ findIndex (\cs -> a `elem` cs) circuits
      let bi = fromJust $ findIndex (\cs -> b `elem` cs) circuits
      let updated =
            if ai == bi
              then circuits -- both already connected
              else merge ai bi circuits -- connect two circuits
      if length updated == 1 -- part 2
        then (updated, Just (a, b))
        else connectCircuits' updated xs
    removeAt i ys =
      let (init, t : tail) = splitAt i ys
       in (t, init ++ tail)
    merge ai bi ys =
      let (t, rest) = removeAt ai ys
          (t', rest') = removeAt (if bi > ai then bi - 1 else bi) rest
       in S.union t t' : rest'

part1 :: Int -> T.Text -> Int
part1 n =
  product
    . take 3
    . sortBy (comparing Down)
    . map length
    . fst
    . connectCircuits
    . take n
    . parse

part2 :: T.Text -> Int
part2 =
  uncurry (*)
    . bimap head head
    . fromJust
    . snd
    . connectCircuits
    . parse
