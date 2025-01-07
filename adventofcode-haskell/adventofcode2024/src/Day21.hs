module Day21 where

import AOC (mkAoCMain)
import AOC.Data.List (groupOn, windows)
import AOC.Data.Tuple (fromList2)
import AOC.Pathing.DFS (dfs)
import Data.Function.Memoize (memoize2)
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 21 part1 part2

parse :: T.Text -> [String]
parse = lines . T.unpack

{-
  7 8 9
  4 5 6
  1 2 3
    0 A
-}

numKeypad :: M.Map Char [Char]
numKeypad =
  M.fromList
    [ ('A', ['0', '3']),
      ('0', ['A', '2']),
      ('1', ['2', '4']),
      ('2', ['0', '1', '3', '5']),
      ('3', ['A', '2', '6']),
      ('4', ['1', '5', '7']),
      ('5', ['2', '4', '6', '8']),
      ('6', ['3', '5', '9']),
      ('7', ['4', '8']),
      ('8', ['5', '7', '9']),
      ('9', ['6', '8'])
    ]

numKeypadDirs :: M.Map (Char, Char) Char
numKeypadDirs =
  M.fromList
    [ (('A', '0'), '<'),
      (('A', '3'), '^'),
      (('0', 'A'), '>'),
      (('0', '2'), '^'),
      (('1', '2'), '>'),
      (('1', '4'), '^'),
      (('2', '0'), 'v'),
      (('2', '1'), '<'),
      (('2', '3'), '>'),
      (('2', '5'), '^'),
      (('3', 'A'), 'v'),
      (('3', '2'), '<'),
      (('3', '6'), '^'),
      (('4', '1'), 'v'),
      (('4', '5'), '>'),
      (('4', '7'), '^'),
      (('5', '2'), 'v'),
      (('5', '4'), '<'),
      (('5', '6'), '>'),
      (('5', '8'), '^'),
      (('6', '3'), 'v'),
      (('6', '5'), '<'),
      (('6', '9'), '^'),
      (('7', '4'), 'v'),
      (('7', '8'), '>'),
      (('8', '5'), 'v'),
      (('8', '7'), '<'),
      (('8', '9'), '>'),
      (('9', '6'), 'v'),
      (('9', '8'), '<')
    ]

{-
    ^ A
  < v >
-}

dirKeypad :: M.Map Char [Char]
dirKeypad =
  M.fromList
    [ ('A', ['^', '>']),
      ('^', ['A', 'v']),
      ('<', ['v']),
      ('v', ['<', '^', '>']),
      ('>', ['v', 'A'])
    ]

dirKeypadDirs :: M.Map (Char, Char) Char
dirKeypadDirs =
  M.fromList
    [ (('A', '^'), '<'),
      (('A', '>'), 'v'),
      (('^', 'A'), '>'),
      (('^', 'v'), 'v'),
      (('<', 'v'), '>'),
      (('v', '<'), '<'),
      (('v', '^'), '^'),
      (('v', '>'), '>'),
      (('>', 'v'), '<'),
      (('>', 'A'), '^')
    ]

minimumsOn :: (Ord b, Eq a) => (a -> b) -> [a] -> [a]
minimumsOn _ [] = []
minimumsOn f xs = head $ groupOn f $ sortOn f xs

paths :: M.Map Char [Char] -> M.Map (Char, Char) Char -> Char -> Char -> [String]
paths m tm start end =
  map (map ((tm M.!) . fromList2) . windows 2 . reverse) $
    minimumsOn length $
      dfs (m M.!) start (== end)

solve :: Int -> String -> Int
solve n = solveMemo 0
  where
    pads = (numKeypad, numKeypadDirs) : replicate n (dirKeypad, dirKeypadDirs)
    solveMemo = memoize2 solve'
    solve' i
      | i == (n + 1) = length
      | otherwise =
          let (m, t) = pads !! i
           in sum
                . map
                  ( ( \(a, b) ->
                        minimum $
                          map (solveMemo (i + 1) . (++ "A")) $
                            paths m t a b
                    )
                      . fromList2
                  )
                . windows 2
                . ('A' :)

complexity :: String -> Int -> Int
complexity code pathLen =
  let numeric = read $ take 3 code
   in pathLen * numeric

part1 :: T.Text -> Int
part1 =
  sum
    . map (\code -> complexity code (solve 2 code))
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . map (\code -> complexity code (solve 25 code))
    . parse
