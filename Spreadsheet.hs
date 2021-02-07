{-# LANGUAGE FlexibleInstances #-}

module Spreadsheet (
  Cell(..), CellID,
  Spreadsheet(..), emptySpreadSheet,
  getCellText, setCellState
  ) where

import Data.Char
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

type CellID = Int

data Cell = Str String | Ref CellID
  deriving (Eq, Show)

type Spreadsheet = Gr Cell Int

emptySpreadSheet :: Spreadsheet
emptySpreadSheet = empty

getCellText :: CellID -> Spreadsheet -> String
getCellText id ss = case lab ss id of
                       Nothing        -> ""
                       Just (Str str) -> str
                       Just (Ref id') -> getCellText id' ss

setCellState :: CellID -> String -> Spreadsheet -> Spreadsheet
setCellState id' str' ss'
  | isLegal id' newRefs ssB' = legalSet id' cell' ssN
  | otherwise = ss'
  where
    legalSet id (Str "") ss
      | null oldRefs = delNode id ss
      | otherwise = case match id ss of
                      (Just (p, _, l, s), cg) -> (p, id, Str "", s) & cg
                      (Nothing,            _) -> error "node does not exist!"
    legalSet _ _ _ = ssN
    oldRefs = suc ss' id'
    ssB = delEdges (zip oldRefs $ repeat id') ss'
    ssB' = case match id' ssB of
             (Just (p, _, l, s), cg) -> (p, id', cell', s) & cg
             (Nothing,            _) -> insNode (id', cell') ssB
    ssN = insEdges (zip3 (repeat id') newRefs $ repeat 1) ssB'
    newRefs = references cell'
    cell' = rep str'

isLegal :: CellID -> [CellID] -> Spreadsheet -> Bool
isLegal id []  _  = True
isLegal id [ref] ss = case sp ref id ss of
                         Nothing -> True
                         Just _  -> False

references :: Cell -> [CellID]
references (Str _)    = []
references (Ref cell) = [cell]

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
      
-- stackoverflow <3
instance Integral a => Enum (a,a) where
  fromEnum (x,y) = fromEnum $ k^2  +  2*j  +  if permuted then 1 else 0
    where
      k = max x y
      j = min x y
      permuted = y>x
  toEnum n =  let k = floor . sqrt $ fromIntegral n
                  (j, permdAdd) = (n-k^2) `divMod` 2
                  permute (x,y) | permdAdd>0  = (y,x)
                                | otherwise    = (x,y)
              in permute (fromIntegral k, fromIntegral j)
