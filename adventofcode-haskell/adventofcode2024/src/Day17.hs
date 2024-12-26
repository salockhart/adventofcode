module Day17 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.List (first2, first3)
import Control.Monad.State (State, execState, get, gets, modify)
import Data.Bifunctor (bimap)
import Data.Bits (xor)
import Data.List (intercalate)
import qualified Data.Text as T

data RAM = RAM
  { _a :: Int,
    _b :: Int,
    _c :: Int,
    _ip :: Int,
    _output :: [Int],
    _instructions :: [Int]
  }
  deriving (Show)

main :: IO ()
main = mkAoCMain 2024 17 part1 part2

parse :: T.Text -> RAM
parse =
  ( \((a, b, c), instructions) ->
      RAM
        { _a = a,
          _b = b,
          _c = c,
          _ip = 0,
          _output = [],
          _instructions = instructions
        }
  )
    . bimap
      (first3 . map (read . drop 12) . lines . T.unpack)
      (map (read . T.unpack) . T.splitOn "," . T.drop 9)
    . first2
    . T.splitOn "\n\n"

getOutput :: RAM -> [Int]
getOutput = reverse . _output . execState execute

execute :: State RAM ()
execute = do
  ram <- get
  let instructions = _instructions ram
      ip = _ip ram

  if ip >= length instructions
    then return ()
    else do
      let op = instructions !! ip
          arg = instructions !! (ip + 1)
      execute' op arg
      execute

advanceIp :: State RAM ()
advanceIp = modify (\ram -> ram {_ip = _ip ram + 2})

execute' :: Int -> Int -> State RAM ()
-- adv
execute' 0 arg = do
  arg' <- combo arg
  a <- gets _a
  modify (\ram -> ram {_a = a `div` (2 ^ arg')})
  advanceIp
-- bxl
execute' 1 arg = do
  modify (\ram -> ram {_b = _b ram `xor` arg})
  advanceIp
-- bst
execute' 2 arg = do
  arg' <- combo arg
  modify (\ram -> ram {_b = arg' `mod` 8})
  advanceIp
-- jnz
execute' 3 arg = do
  a <- gets _a
  if a == 0 then advanceIp else modify (\ram -> ram {_ip = arg})
-- bxc
execute' 4 _ = do
  b <- gets _b
  c <- gets _c
  modify (\ram -> ram {_b = b `xor` c})
  advanceIp
-- out
execute' 5 arg = do
  arg' <- combo arg
  modify (\ram -> ram {_output = (arg' `mod` 8) : _output ram})
  advanceIp
--  bdv
execute' 6 arg = do
  arg' <- combo arg
  a <- gets _a
  modify (\ram -> ram {_b = a `div` (2 ^ arg')})
  advanceIp
--  cdv
execute' 7 arg = do
  arg' <- combo arg
  a <- gets _a
  modify (\ram -> ram {_c = a `div` (2 ^ arg')})
  advanceIp
execute' op _ = error ("invalid op" ++ show op)

combo :: Int -> State RAM Int
combo 0 = return 0
combo 1 = return 1
combo 2 = return 2
combo 3 = return 3
combo 4 = gets _a
combo 5 = gets _b
combo 6 = gets _c
combo x = error ("invalid combo: " ++ show x)

part1 :: T.Text -> String
part1 =
  intercalate ","
    . map show
    . getOutput
    . parse

part2 :: T.Text -> Int
part2 =
  minimum
    . (\ram -> solve ram 0 (reverse $ _instructions ram))
    . parse
  where
    solve _ a [] = [a]
    solve ram a (i : is) = do
      let as = map (\x -> (a * 8) + x) [0 .. 7]
      let outs = map (\a' -> (a', getOutput (ram {_a = a'}))) as
      let outs' = filter (\(_, o) -> head o == i) outs
      let as' = map fst outs'
      concatMap (\a' -> solve ram a' is) as'
