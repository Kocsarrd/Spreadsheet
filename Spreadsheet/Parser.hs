module Spreadsheet.Parser (rep) where

import Control.Applicative (liftA2)
import Data.Char
import Data.Functor ((<&>))
import Data.Ratio
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Spreadsheet.Spreadsheet

rep :: String -> Cell
rep str = case parse (cellParser str) "" str of
  Right cell -> cell
  _          -> error "parse error"

cellParser :: String -> Parser Cell
cellParser str = try (formula str)
                  <|> (Number <$> try number)
                  <|> (Str <$> many anyChar)


formula :: String -> Parser Cell
formula str = do
  spaces *> char '=' *> char '§'
  n <- letterToNum <$> (toUpper <$> letter)
  m <- cellNum
  char '§' <* spaces <* notFollowedBy anyChar
  return $ Ref (fromEnum (n,m)) str

letterToNum :: Char -> Int
letterToNum c = fromEnum c - 65

cellNum :: Parser Int
cellNum = read <$> many1 digit

number :: Parser Rational
number = fmap rd $ liftA2 (++) integer decimal <* spaces <* notFollowedBy anyChar
  where
    rd = flip approxRational 0 . (read :: String -> Float)
    decimal  = option "" $ liftA2 (:) (char '.') digits
    digits = many1 digit
    plus = char '+' *> digits
    minus = liftA2 (:) (char '-') digits
    integer = plus <|> minus <|> digits

code :: Parser String
code = many1 $ satisfy (/= '§')

-- currently a cell row identifier may only contain a single letter
reference :: Parser [CellID]
reference = char '§' *> (try listRef <|> singleRef) <* char '§'
  where
    singleRef = ref <&> fromEnum <&> pure
    listRef = do
      (r1,c1) <- ref
      char ':'
      (r2,c2) <- ref
      return [fromEnum (r,c) | r <- [r1..r2], c <- [c1..c2]]
    ref = do
      n <- letter <&> toUpper <&> letterToNum
      m <- cellNum
      return $ (n,m)
