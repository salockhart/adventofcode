module Day09 (main, part1, part2) where

import AOC (mkAoCMain)
import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

data DiscRange = Data Int Int | Free Int deriving (Show)

main :: IO ()
main = mkAoCMain 2024 09 part1 part2

parse :: T.Text -> [Int]
parse =
  map ((read :: String -> Int) . (: []))
    . T.unpack
    . T.strip

parseDiscRanges :: [Int] -> [DiscRange]
parseDiscRanges = parseData 0
  where
    parseData _ [] = []
    parseData i (x : xs) = Data i x : parseFree (i + 1) xs
    parseFree _ [] = []
    parseFree i (x : xs) = Free x : parseData i xs

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [a] = [a]
everyOther (a : _ : xs) = a : everyOther xs

takeBlocks :: Int -> [(Int, Int)] -> ([Int], [(Int, Int)])
takeBlocks n ((i, count) : xs)
  | n == count = (replicate n i, xs)
  | n < count = (replicate n i, (i, count - n) : xs)
  | n > count =
      let (a, b) = takeBlocks (n - count) xs
       in (replicate count i ++ a, b)
takeBlocks _ _ = ([], [])

popDataBlock :: Int -> [DiscRange] -> (DiscRange, [DiscRange])
popDataBlock i (db@(Free _) : xs) = second (db :) $ popDataBlock i xs
popDataBlock i (db@(Data i' x) : xs)
  | i == i' = (db, Free x : xs)
  | otherwise = second (db :) $ popDataBlock i xs
popDataBlock _ [] = error "No such block"

shiftDataBlock :: DiscRange -> [DiscRange] -> [DiscRange]
shiftDataBlock _ [] = []
shiftDataBlock (Free _) _ = error "Cannot shift Free"
shiftDataBlock db@(Data _ n) ((Free n') : bs)
  | n == n' = db : bs
  | n < n' = db : Free (n' - n) : bs
  | otherwise = Free n' : shiftDataBlock db bs
shiftDataBlock db (db' : bs) = db' : shiftDataBlock db bs

collapseFreeBlocks :: [DiscRange] -> [DiscRange]
collapseFreeBlocks (Free a : Free b : xs) = Free (a + b) : collapseFreeBlocks xs
collapseFreeBlocks (Free 0 : xs) = collapseFreeBlocks xs
collapseFreeBlocks (x : xs) = x : collapseFreeBlocks xs
collapseFreeBlocks [] = []

checksum :: [DiscRange] -> Int
checksum = checksum' 0
  where
    checksum' x ((Data _ 0) : bs) = checksum' x bs
    checksum' x ((Data i n) : bs) = x * i + checksum' (x + 1) (Data i (n - 1) : bs)
    checksum' x ((Free n) : bs) = checksum' (x + n) bs
    checksum' _ _ = 0

part1 :: T.Text -> Int
part1 = solve . parse
  where
    solve xs =
      let bs = zip [0 :: Int ..] $ everyOther xs
       in sum $ zipWith (*) [0 ..] (solve' xs bs)
      where
        solve' [] _ = []
        solve' (x : xs') bs =
          let (xs'', bs') = takeBlocks x bs
           in xs'' ++ solve' xs' (reverse bs')

part2 :: T.Text -> Int
part2 =
  solve
    . parseDiscRanges
    . parse
  where
    solve bs =
      let is = reverse (mapMaybe dataBlockIndex bs)
       in checksum $ foldl solve2' bs is
      where
        dataBlockIndex (Data i _) = Just i
        dataBlockIndex _ = Nothing
        solve2' bs' i = case popDataBlock i bs' of
          (Data _ n, bs'') -> collapseFreeBlocks $ shiftDataBlock (Data i n) bs''
          _ -> error "No such block"
