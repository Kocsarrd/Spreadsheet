module Spreadsheet.Parser (rep) where

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Spreadsheet.Spreadsheet

rep :: String -> Cell
rep str = case parse cellParser "" str of
  Right cell -> cell
  _          -> error "parse error"

cellParser :: Parser Cell
cellParser = try refParser <|> (Str <$> many anyChar)
  where
    refParser :: Parser Cell
    refParser = do
      spaces
      char '§'
      n <- letterToNum <$> (toUpper <$> letter)
      m <- number
      char '§'
      return $ Ref $ fromEnum (n,m)

    letterToNum :: Char -> Int
    letterToNum c = fromEnum c - 65

    number :: Parser Int
    number = read <$> many1 digit
