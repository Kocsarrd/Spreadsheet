module Eval.CommandLine (parseCommand, ClCommand(..)) where

import Data.Char
import Data.Functor ((<&>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Spreadsheet.Types (CellID)

data ClCommand = ClGhci String
               | ClMv [(CellID, CellID)]
               | ClCp [(CellID, CellID)]
  deriving (Eq,Show)

parseCommand :: String -> Maybe ClCommand
parseCommand = either (const Nothing) Just . parse commandP "" 

commandP :: Parser ClCommand
commandP = spaces *> (clGhciP <|> clCpP <|> clMvP)

clGhciP :: Parser ClCommand
clGhciP = fmap ClGhci $ char 'g' *> many1 space *> many anyChar

clCpP :: Parser ClCommand
clCpP = fmap ClCp $ string "cp" *> refPairsP

clMvP :: Parser ClCommand
clMvP = fmap ClMv $ string "mv" *> refPairsP

-- this contains some copypasta from Spreadsheet.Parser
-- some day it will be abstracted away
refPairsP :: Parser [(CellID, CellID)]
refPairsP = do
  spaces
  ((r1,c1),(r2,c2)) <- refPair
  spaces
  (r3,c3) <- char '§' *> ref <* char '§'
  let d = (r3-r1, c3-c1)
  spaces
  notFollowedBy anyChar
  pure [(fromEnum (r,c), fromEnum ((r,c) `shift` d)) | r <- [r1..r2], c <- [c1..c2]]
  where
    refPair = do
      char '§'
      r1 <- ref
      char ':'
      r2 <- ref
      char '§'
      pure (r1,r2)
    ref = do
      n <- letter <&> toUpper <&> letterToNum
      m <- cellNum
      pure $ (n,m)
    cellNum :: Parser Int
    cellNum = read <$> many1 digit
    letterToNum c = fromEnum c - 65
    (x1,y1) `shift` (x2,y2) = (x1+x2,y1+y2)
  
{-
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

-}
