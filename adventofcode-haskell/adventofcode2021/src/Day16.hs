module Day16 (main, part1, part2) where

import AOC (btoi, chunks, dbg, slice)
import Control.Monad.Combinators (manyTill_)
import Data.Either (fromRight)
import Data.List.Split (chunksOf, splitEvery)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (ErrorItem (EndOfInput, Label), MonadParsec (failure), Parsec, choice, count, getInput, parse, some, try, unexpected)
import Text.Megaparsec.Char (binDigitChar, char, string)

data Packet
  = LiteralPacket Int Int Int
  | OperatorPacket Int Int [Packet]
  deriving (Show)

type Parser = Parsec Void String

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parseHex '0' = "0000"
parseHex '1' = "0001"
parseHex '2' = "0010"
parseHex '3' = "0011"
parseHex '4' = "0100"
parseHex '5' = "0101"
parseHex '6' = "0110"
parseHex '7' = "0111"
parseHex '8' = "1000"
parseHex '9' = "1001"
parseHex 'A' = "1010"
parseHex 'B' = "1011"
parseHex 'C' = "1100"
parseHex 'D' = "1101"
parseHex 'E' = "1110"
parseHex 'F' = "1111"
parseHex _ = error "cannot parse hex"

btoi' = btoi . map (\x -> read [x])

parsePacket :: Parser Packet
parsePacket = do
  choice [try parseLiteral, parseOperator]

parseLiteral :: Parser Packet
parseLiteral = do
  version <- count 3 binDigitChar
  typeID <- string "100"
  (valueInit, valueLast) <- manyTill_ parseLiteralMiddle parseLiteralEnd
  let value = concat (valueInit ++ [valueLast])
  return $ LiteralPacket (btoi' version) (btoi' typeID) (btoi' value)
  where
    parseLiteralMiddle :: Parser String
    parseLiteralMiddle = do
      char '1'
      parseLiteral
    parseLiteralEnd :: Parser String
    parseLiteralEnd = do
      char '0'
      parseLiteral
    parseLiteral :: Parser String
    parseLiteral = do
      count 4 binDigitChar

parseOperator :: Parser Packet
parseOperator = do
  version <- count 3 binDigitChar
  typeID <- count 3 binDigitChar
  packets <- choice [try parsePacketsByLength, parsePacketsByNum]
  return $ OperatorPacket (btoi' version) (btoi' typeID) packets
  where
    parsePacketsByLength :: Parser [Packet]
    parsePacketsByLength = do
      char '0'
      numBits <- count 15 binDigitChar
      bits <- count (btoi' numBits) binDigitChar
      let packets = parse (some parsePacket) "day 16 - inner" bits
      return $ fromRight [] packets
    parsePacketsByNum :: Parser [Packet]
    parsePacketsByNum = do
      char '1'
      numPackets <- count 11 binDigitChar
      count (btoi' numPackets) parsePacket

sumVersions :: Packet -> Int
sumVersions (LiteralPacket x _ _) = x
sumVersions (OperatorPacket x _ ps) = x + sum (map sumVersions ps)

evaluate :: Packet -> Int
evaluate (LiteralPacket _ _ x) = x
evaluate (OperatorPacket _ 0 ps) = sum (map evaluate ps)
evaluate (OperatorPacket _ 1 ps) = product (map evaluate ps)
evaluate (OperatorPacket _ 2 ps) = minimum (map evaluate ps)
evaluate (OperatorPacket _ 3 ps) = maximum (map evaluate ps)
evaluate (OperatorPacket _ 5 (pa : pb : _)) = if evaluate pa > evaluate pb then 1 else 0
evaluate (OperatorPacket _ 6 (pa : pb : _)) = if evaluate pa < evaluate pb then 1 else 0
evaluate (OperatorPacket _ 7 (pa : pb : _)) = if evaluate pa == evaluate pb then 1 else 0
evaluate _ = error "cannot evaluate"

part1 :: String -> String
part1 =
  show
    . sumVersions
    . fromRight (LiteralPacket 0 0 0)
    . parse parsePacket "day 16"
    . concatMap parseHex

part2 :: String -> String
part2 =
  show
    . evaluate
    . fromRight (LiteralPacket 0 0 0)
    . parse parsePacket "day 16"
    . concatMap parseHex
