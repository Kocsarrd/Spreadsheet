module Spreadsheet.Interface
  (
  emptySpreadsheet,
  getCellText, setCellState
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query

import Spreadsheet.Spreadsheet
import Spreadsheet.Parser

emptySpreadsheet :: Spreadsheet
emptySpreadsheet = empty

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

