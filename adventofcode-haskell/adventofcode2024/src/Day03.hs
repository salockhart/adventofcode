module Day03 (main, part1, part2) where

import AOC (mkAoCMain)
import qualified Data.Text as T
import Text.Regex.PCRE ((=~))

data Instruction
  = Mul Int Int
  | Do
  | Dont
  deriving (Show, Eq)

main :: IO ()
main = mkAoCMain 2024 03 part1 part2

regex :: String
regex = "((mul)\\((\\d+),(\\d+)\\))|(do\\(\\))|(don't\\(\\))"

parse :: String -> [Instruction]
parse line = map extract (line =~ regex :: [[String]])
  where
    extract :: [String] -> Instruction
    extract (_ : _ : "mul" : a : b : _) = Mul (read a) (read b)
    extract ("do()" : _) = Do
    extract ("don't()" : _) = Dont
    extract _ = error "parse error"

part1 :: T.Text -> Int
part1 = solve . parse . T.unpack
  where
    solve ((Mul a b) : is) = a * b + solve is
    solve (Do : is) = solve is
    solve (Dont : is) = solve is
    solve [] = 0

part2 :: T.Text -> Int
part2 = solve . parse . T.unpack
  where
    solve ((Mul a b) : is) = a * b + solve is
    solve (Do : is) = solve is
    solve (Dont : is) = solve $ dropWhile (/= Do) is
    solve [] = 0
