module Day09 (main, part1, part2) where

import Data.Either (rights)
import Data.List (nub)
import Data.Tuple.Utils (fst3)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, parse, some)
import Text.Megaparsec.Char (char, digitChar, letterChar, space1, string)

type Parser = Parsec Void String

type Distance = (String, String, Int)

type Path = [(String, Int)]

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parseDistance :: Parser Distance
parseDistance = do
  a <- some letterChar
  between space1 space1 (string "to")
  b <- some letterChar
  between space1 space1 (char '=')
  i <- some digitChar
  return (a, b, read i)

makeBiDirectional :: [Distance] -> [Distance]
makeBiDirectional = concatMap (\(a, b, i) -> [(a, b, i), (b, a, i)])

traversals :: [Distance] -> [Path]
traversals distances = concat [traversals' [(b, i), (a, 0)] distances | (a, b, i) <- distances]
  where
    numNodes :: Int
    numNodes = (length . nub . map fst3) distances
    traversals' :: Path -> [Distance] -> [Path]
    traversals' from distances
      | length from == numNodes = [from]
      | otherwise = do
        let visited = map fst from
        let current = fst $ head from
        let possibles = filter (\(a, b, _) -> a == current && b `notElem` visited) distances
        concat [traversals' ((b, i) : from) distances | (_, b, i) <- possibles]

part1 :: String -> String
part1 =
  show
    . minimum
    . map (sum . map snd)
    . traversals
    . makeBiDirectional
    . rights
    . map (parse parseDistance "day 9")
    . lines

part2 :: String -> String
part2 =
  show
    . maximum
    . map (sum . map snd)
    . traversals
    . makeBiDirectional
    . rights
    . map (parse parseDistance "day 9")
    . lines
