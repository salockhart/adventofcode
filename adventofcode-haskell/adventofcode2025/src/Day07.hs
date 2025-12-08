module Day07 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Function.Memoize (Memoizable (memoize))
import Data.List (find, nub, uncons)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2025 07 part1 part2

parse :: T.Text -> ((Coord, [Coord]), M.Map Coord [Coord])
parse =
  second M.fromList
    . fromJust
    . uncons
    . emitBeams
    . concat
    . zipWith (\i -> map (first (,i))) [0 :: Int ..]
    . map (filter ((`elem` ['S', '^']) . snd) . zip [0 :: Int ..])
    . lines
    . T.unpack
  where
    emitBeams [] = []
    emitBeams ((c@(x, _), 'S') : xs) = do
      let nn = fst $ fromJust $ find (\((x', _), _) -> x' == x) xs
      (c, [nn]) : emitBeams xs
    emitBeams ((c@(x, _), _) : xs) = do
      let nl = find (\((x', _), _) -> x' == x - 1) xs
      let nr = find (\((x', _), _) -> x' == x + 1) xs
      (c, map fst $ catMaybes [nl, nr]) : emitBeams xs

part1 :: T.Text -> Int
part1 =
  (\x -> x - 1) -- don't count the S
    . length
    . uncurry (flip traceBeam)
    . parse
  where
    traceBeam bm = traceBeamMemo
      where
        traceBeamMemo = memoize traceBeam'
        traceBeam' (c, []) = [c]
        traceBeam' (c, ns) =
          c : nub (concatMap (\n -> traceBeamMemo (n, bm M.! n)) ns)

part2 :: T.Text -> Int
part2 =
  uncurry (flip traceBeam)
    . parse
  where
    traceBeam bm = traceBeamMemo
      where
        traceBeamMemo = memoize traceBeam'
        traceBeam' (_, []) = 1
        traceBeam' (_, ns) =
          1 + sum (map (\n -> traceBeamMemo (n, bm M.! n)) ns)
