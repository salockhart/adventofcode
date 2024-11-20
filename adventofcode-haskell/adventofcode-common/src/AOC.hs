module AOC
  ( applyN,
    btoi,
    dbg,
    mkAoCMain,
    notImplemented,
  )
where

import Advent (AoC (AoCInput, AoCSubmit), AoCUserAgent (AoCUserAgent, _auaEmail, _auaRepo), Part (Part1, Part2), defaultAoCOpts, mkDay_, runAoC)
import Control.Exception (Exception, throw)
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Text (Text, pack, unpack)
import Debug.Trace (trace)
import System.Environment (getEnv)

-- Advent API

data AoCException = NotImplementedException deriving (Show)

instance Exception AoCException

notImplemented :: a
notImplemented = throw NotImplementedException

aocUserAgent :: AoCUserAgent
aocUserAgent =
  AoCUserAgent
    { _auaRepo = pack "salockhart/adventofcode",
      _auaEmail = pack "alex@lockhart.dev"
    }

mkAoCMain :: (Show a) => (Show b) => Integer -> Integer -> (Text -> a) -> (Text -> b) -> IO ()
mkAoCMain year day part1Solver part2Solver = do
  token <- getToken

  let runner = runAoC' token
  input <- runner getInput >>= handleInputError

  reportAnswer Part1 (pack $ show $ part1Solver input) runner
  reportAnswer Part2 (pack $ show $ part2Solver input) runner

  return ()
  where
    getToken = getEnv "AOC_SESSION"
    runAoC' key = runAoC (defaultAoCOpts aocUserAgent year key)
    getInput = AoCInput (mkDay_ day)
    handleInputError = either (error . show) return
    getSubmit = AoCSubmit (mkDay_ day)
    reportAnswer part output runner = do
      putStr (show part)
      putStr ": "
      putStr (unpack output)
      putStr " ("
      submissionResult <- runner (getSubmit part (unpack output)) >>= handleSubmitError <&> show
      putStr submissionResult
      putStrLn ")"
    handleSubmitError = either (error . show) (return . snd)

-- Utils

applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) id (List.replicate n f)

dbg :: (Show a) => a -> a
dbg x = trace (show x) x

btoi :: [Int] -> Int
btoi = sum . zipWith (\i x -> x * (2 ^ i)) [0 :: Int ..] . reverse
