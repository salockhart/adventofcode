module Day1 (main, part1, part2) where

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

findPair :: Int -> [Int] -> [(Int, Int)]
findPair _ [] = []
findPair sum (start : list) = [(start, y) | y <- list, start + y == sum] ++ findPair sum list

tackOn :: Int -> (Int, Int) -> (Int, Int, Int)
tackOn a (b, c) = (a, b, c)

findTriple :: Int -> [Int] -> [(Int, Int, Int)]
findTriple _ [] = []
findTriple sum (start : list) = map (tackOn start) (findPair (sum - start) list) ++ findTriple sum list

multiply :: (Int, Int) -> Int
multiply (a, b) = a * b

multiply3 :: (Int, Int, Int) -> Int
multiply3 (a, b, c) = a * b * c

part1 :: String -> Int
part1 input = head $ map multiply $ findPair 2020 $ map read $ lines input

part2 :: String -> Int
part2 input = head $ map multiply3 $ findTriple 2020 $ map read $ lines input
