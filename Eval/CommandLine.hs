module Eval.CommandLine (parseCommand, ClCommand(..)) where

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data ClCommand = ClGhci String
  deriving (Eq,Show)

parseCommand :: String -> Maybe ClCommand
parseCommand = either (const Nothing) Just . parse commandP "" 

commandP :: Parser ClCommand
commandP = clGhciP

clGhciP :: Parser ClCommand
clGhciP = fmap ClGhci $ spaces *> char 'g' *> many1 space *> many anyChar
