module Spreadsheet.Interface
  (
  emptySpreadsheet,
  getCellText, setCellState
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query
import Lens.Micro.Platform hiding ((&))

import Spreadsheet.Spreadsheet
import Spreadsheet.Parser

emptySpreadsheet :: Spreadsheet
emptySpreadsheet = SS empty Nothing

getCellText :: CellID -> Spreadsheet -> String
getCellText id ss = case lab (ss^.sheet) id of
                       Nothing        -> ""
                       Just (Str str) -> str
                       Just (Ref id') -> getCellText id' ss

setCellState :: CellID -> String -> Spreadsheet -> Spreadsheet
setCellState id' str' ss'
  | isLegal id' newRefs ssB' = over sheet (legalSet id' cell') ssN
  | otherwise = ss'
  where
    legalSet id (Str "") sh
      | null oldRefs = delNode id sh
      | otherwise = case match id sh of
                      (Just (p, _, l, s), cg) -> (p, id, Str "", s) & cg
                      (Nothing,            _) -> error "node does not exist!"
    legalSet _ _ _ = ssN^.sheet
    oldRefs = suc (ss'^.sheet) id'
    ssB = over sheet (delEdges (zip oldRefs $ repeat id')) ss'
    ssB' = case match id' $ ssB^.sheet of
             (Just (p, _, l, s), cg) -> set sheet ((p, id', cell', s) & cg) ssB
             (Nothing,            _) -> over sheet (insNode (id', cell')) ssB
    ssN = over sheet (insEdges (zip3 (repeat id') newRefs $ repeat 1)) ssB'
    newRefs = references cell'
    cell' = rep str'

isLegal :: CellID -> [CellID] -> Spreadsheet -> Bool
isLegal id []  _  = True
isLegal id [ref] ss = case sp ref id $ ss^.sheet of
                         Nothing -> True
                         Just _  -> False

references :: Cell -> [CellID]
references (Str _)    = []
references (Ref cell) = [cell]

