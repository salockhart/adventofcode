module Day24 (main, part1, part2) where

import AOC (mkAoCMain, notImplemented)
import AOC.Data.Tuple (fromList2)
import Control.Monad.State (State, evalState, get, modify)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits (Bits (xor), (.&.), (.|.))
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Regex.PCRE ((=~))

type Gate = (String, String, String)

type StateV = (M.Map String Int, M.Map String Gate)

main :: IO ()
main = mkAoCMain 2024 24 part1 part2

parse :: T.Text -> (M.Map String Int, M.Map String Gate)
parse =
  bimap (parseLines wirePattern parseWire) (parseLines gatePattern parseGate)
    . fromList2
    . T.splitOn (T.pack "\n\n")

parseLines :: String -> ([String] -> (String, a)) -> T.Text -> M.Map String a
parseLines pattern p = M.fromList . map (p . head . (=~ pattern)) . lines . T.unpack

wirePattern :: String
wirePattern = "(\\w+): (\\d)" :: String

parseWire :: [String] -> (String, Int)
parseWire [_, a, r] = (a, read r)
parseWire x = error ("parseWire:" ++ show x)

gatePattern :: String
gatePattern = "(\\w+) (AND|XOR|OR) (\\w+) -> (\\w+)"

parseGate :: [String] -> (String, Gate)
parseGate [_, a, "AND", b, r] = (r, (a, "AND", b))
parseGate [_, a, "XOR", b, r] = (r, (a, "XOR", b))
parseGate [_, a, "OR", b, r] = (r, (a, "OR", b))
parseGate x = error ("parseGate:" ++ show x)

applyGate :: String -> Int -> Int -> Int
applyGate "AND" = (.&.)
applyGate "XOR" = xor
applyGate "OR" = (.|.)
applyGate x = error ("applyGate:" ++ x)

evalWire :: String -> State StateV Int
evalWire wire = do
  (wires, gates) <- get
  case M.lookup wire wires of
    Just v -> return v
    Nothing -> do
      let (a, fn, b) = gates M.! wire
      a' <- evalWire a
      b' <- evalWire b
      let v = applyGate fn a' b'
      let wires' = M.insert wire v wires
      let state' = (wires', gates)
      modify (const state')
      return v

solve :: Char -> M.Map String Int -> M.Map [Char] Gate -> [Int]
solve c wires gates =
  let ts = filter ((== c) . head) (M.keys wires ++ M.keys gates)
   in mapM evalWire ts `evalState` (wires, gates)

btoa :: (Integral a) => [a] -> a
btoa [] = 0
btoa (x : xs) = x + 2 * btoa xs

part1 :: T.Text -> Int
part1 =
  btoa
    . uncurry (solve 'z')
    . parse

-- Part 2 completed by hand
-- The wire diagram is a collection of full adders
-- beginning with a single half adder
--
-- Examples:
-- x00 XOR y00 -> z00
-- x00 AND y00 -> c00 (carry 00)

-- xNN XOR yNN -> aNN
-- aNN XOR c(NN-1) -> zNN
-- xNN AND yNN -> bNN
-- c(NN-1) AND aNN -> dNN
-- bNN OR dNN -> cNN

part2 :: T.Text -> String
part2 = notImplemented
