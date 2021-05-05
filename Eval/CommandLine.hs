module Eval.CommandLine (parseCommand, ClCommand(..), shiftCode) where

import Control.Applicative (liftA2)
import Data.Char
import Data.Functor ((<&>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Spreadsheet.Types (CellID)

data ClCommand = ClGhci String
               | ClMv ((Int,Int),[(CellID, CellID)])
               | ClCp ((Int,Int),[(CellID, CellID)])
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
refPairsP :: Parser ((Int,Int),[(CellID, CellID)])
refPairsP = do
  spaces
  ((r1,c1),(r2,c2)) <- refPair
  spaces
  (r3,c3) <- char '§' *> ref <* char '§'
  let d = (r3-r1, c3-c1)
  spaces
  notFollowedBy anyChar
  pure (d,[(fromEnum (r,c), fromEnum ((r,c) `shift` d)) | r <- [r1..r2], c <- [c1..c2]])
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
    
  
shiftCode :: (Int,Int) -> String -> String
shiftCode (s1,s2) str = case parse shiftP "" str of
                          Left _ -> error "wut?"
                          Right res -> res
  where
    shiftP = concat <$> many (try refsParser <|> many1 (satisfy (/='§')))
    refsParser = char '§' <:> (try lRef <|> sRef) <++> (pure <$> char '§')
    sRef = ((char '$' <:> (pure <$> anyChar)) <|> (anyChar <&> shiftChar <&> pure))
           <++> ((char '$' <:> many (satisfy isDigit)) <|> (sucStr <$> many (satisfy isDigit)))
    lRef = sRef <++> (char ':' <:> sRef)
    (<++>) = liftA2 (++)
    (<:>) = liftA2 (:)
    shiftChar :: Char -> Char
    shiftChar = toEnum.(+s1).fromEnum
    sucStr = show.(+s2).(read :: String -> Int)
