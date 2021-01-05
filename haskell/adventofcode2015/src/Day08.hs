module Day08 (main, part1, part2) where

import Data.Either (fromRight)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, count, many, parse, some)
import Text.Megaparsec.Char (alphaNumChar, char, hexDigitChar, letterChar, string)

type Parser = Parsec Void String

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

unescapeSlashChar :: Parser Char
unescapeSlashChar = do
  string "\\\\"
  return '\\'

escapeSlashChar :: Parser String
escapeSlashChar = do
  char '\\'
  return "\\\\"

unescapeQuoteChar :: Parser Char
unescapeQuoteChar = do
  string "\\\""
  return '\"'

escapeQuoteChar :: Parser String
escapeQuoteChar = do
  char '\"'
  return "\\\""

unescapeHexChar :: Parser Char
unescapeHexChar = do
  string "\\x"
  count 2 hexDigitChar
  return '.'

unescape :: Parser String
unescape = do
  many $
    choice
      [ unescapeSlashChar,
        unescapeQuoteChar,
        letterChar,
        unescapeHexChar
      ]

escape :: Parser [String]
escape = do
  many $
    choice
      [ escapeSlashChar,
        escapeQuoteChar,
        some alphaNumChar
      ]

trim :: String -> [Char]
trim str = take (length str - 2) (drop 1 str)

part1 :: String -> String
part1 = show . sum . map solve . lines
  where
    solve str = do
      let stringChars = length str
      let memChars = length $ fromRight "" $ parse unescape "day 8" $ trim str
      stringChars - memChars

part2 :: String -> String
part2 = show . sum . map solve . lines
  where
    solve str = do
      let stringChars = length str
      let memChars = length $ concat $ fromRight [] $ parse escape "day 8" str
      (memChars + 2) - stringChars
