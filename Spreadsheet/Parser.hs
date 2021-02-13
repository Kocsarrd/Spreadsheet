module Spreadsheet.Parser (rep) where

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Spreadsheet.Spreadsheet

rep :: String -> Cell
rep str = case parse (cellParser str) "" str of
  Right cell -> cell
  _          -> error "parse error"

cellParser :: String -> Parser Cell
cellParser str = try refParser <|> (Str <$> many anyChar)
  where
    refParser :: Parser Cell
    refParser = do
      spaces
      char '§'
      n <- letterToNum <$> (toUpper <$> letter)
      m <- number
      char '§'
      return $ Ref (fromEnum (n,m)) str

    letterToNum :: Char -> Int
    letterToNum c = fromEnum c - 65

    number :: Parser Int
    number = read <$> many1 digit
