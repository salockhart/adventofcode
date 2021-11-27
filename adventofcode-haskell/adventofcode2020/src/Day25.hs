module Day25 (main, part1) where

main :: IO ()
main = interact (show . part1)

execute :: Int -> Int -> Int
execute val subj = (val * subj) `mod` 20201227

findLoopSize :: Int -> Int -> Int
findLoopSize subj targ = findLoopSize' subj subj targ
  where
    findLoopSize' val subj targ =
      if val == targ
        then 1
        else 1 + findLoopSize' (execute val subj) subj targ

loopFor :: Int -> Int -> Int -> Int
loopFor 0 val _ = val
loopFor round val subj = iterate (`execute` subj) val !! round

solve :: [Int] -> Int
solve keys@[_, door] = do
  let [cardLoop, _] = map (findLoopSize 7) keys
  loopFor cardLoop 1 door

part1 :: String -> String
part1 = show . solve . map read . lines
