module Day15 (main, part1, part2) where

import AOC.Data.String (splitOn)
import Control.Monad
import Control.Monad.ST
import Data.List as List
import Data.Vector.Unboxed.Mutable as MVector

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> ([(Int, Int)], Int, Int)
parse str = do
  let vals = map Prelude.read $ splitOn "," str
  let memory = List.zip vals [1 ..]
  (memory, last vals, List.length memory)

initVector :: Int -> [(Int, Int)] -> ST s (MVector.MVector s Int)
initVector n init = do
  result <- MVector.replicate 30000000 (-1)
  Control.Monad.forM_ init $ \(k, v) -> do
    write result k v
  return result

run :: Int -> (MVector.MVector s Int) -> Int -> Int -> ST s Int
run n v last pos
  | pos == n = pure last
  | otherwise = do
      prevPos <- MVector.read v last
      diff <- pure $ if prevPos == (-1) then 0 else (pos - prevPos)
      MVector.write v last pos
      run n v diff (pos + 1)

getNth :: Int -> [(Int, Int)] -> Int -> Int -> Int
getNth n init last pos =
  runST $ do
    v <- initVector n init
    run n v last pos

part1 :: String -> String
part1 = show . solve . parse
  where
    solve (memo, last, pos) = getNth 2020 memo last pos

part2 :: String -> String
part2 = show . solve . parse
  where
    solve (memo, last, pos) = getNth 30000000 memo last pos
