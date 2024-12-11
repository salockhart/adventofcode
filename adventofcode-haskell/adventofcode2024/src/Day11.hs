module Day11 (main, part1, part2) where

import AOC (mkAoCMain)
import Control.Monad.State (State, evalState, get, modify)
import qualified Data.Map as M
import qualified Data.Text as T

type StateMap a = State (M.Map (Int, Int) Int) a

main :: IO ()
main = mkAoCMain 2024 11 part1 part2

parse :: T.Text -> [Int]
parse = map (read . T.unpack) . T.splitOn " " . T.strip

solve :: Int -> [Int] -> Int
solve n xs = sum $ evalState (mapM (step n) xs) M.empty

step :: Int -> Int -> StateMap Int
step 0 _ = return 1
step n 0 = getOrCompute (n - 1) 1
step n x
  | even (length (show x)) =
      let x' = show x
          (a, b) = splitAt (length x' `div` 2) x'
       in do
            l <- getOrCompute (n - 1) (read a)
            r <- getOrCompute (n - 1) (read b)
            return (l + r)
  | otherwise = getOrCompute (n - 1) (x * 2024)

getOrCompute :: Int -> Int -> StateMap Int
getOrCompute n' x' = do
  m <- get
  case M.lookup (n', x') m of
    Just v -> return v
    Nothing -> do
      r <- step n' x'
      modify (M.insert (n', x') r)
      return r

part1 :: T.Text -> Int
part1 = solve 25 . parse

part2 :: T.Text -> Int
part2 = solve 75 . parse
