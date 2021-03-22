module Spreadsheet.Parser (rep) where

import Control.Applicative (liftA2)
import Data.Char
import Data.Functor ((<&>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Spreadsheet.Types

-- cell representation of user giver string
-- an error may only occur when head str == '='
rep :: String -> Cell
rep str = case parse (cellP str') "" str' of
  Right cell -> cell
  Left  err  -> For $ Formula str (Left FNoParse) Nothing
  where
    str' = if isStringLiteral str then init $ tail str else str
    isStringLiteral (x:y:xs) = x == '"' && last (y:xs) == '"'
    isStringLiteral _ = False

cellP :: String -> Parser Cell
cellP str = try numberP
            <|> formulaP str
            <|> (many anyChar <&> Str <&> Val)

-- parse a numeric value
numberP :: Parser Cell
numberP = double <&> Number <&> Val
  where
    double = fmap rd $ spaces *> liftA2 (++) integer decimal <* spaces <* notFollowedBy anyChar
    rd = read :: String -> Double
    decimal  = option "" $ liftA2 (:) (char '.') digits
    digits = many1 digit
    plus = char '+' *> digits
    minus = liftA2 (:) (char '-') digits
    integer = plus <|> minus <|> digits

-- parse a formula
formulaP :: String -> Parser Cell
formulaP str = char '=' *> (Just <$> many1 (refsP <|> codeP)) <&> Formula str (Left FNoCache) <&> For

-- parse Code (ForPiece)
codeP :: Parser ForPiece
codeP = fmap Code $ many1 $ satisfy (/= '§')

-- parse Refs (ForPiece)
-- currently a cell row identifier may only contain a single letter
refsP :: Parser ForPiece
refsP = fmap Refs $ char '§' *> (try listRef <|> singleRef) <* char '§'
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
    cellNum :: Parser Int
    cellNum = read <$> many1 digit
    letterToNum c = fromEnum c - 65
