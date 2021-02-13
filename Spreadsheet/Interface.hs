module Spreadsheet.Interface
  (
  emptySpreadsheet,
  getCellText, getCellCode, setCellState,
  getSelected, setSelected
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query
import Lens.Micro.Platform hiding ((&))

import Spreadsheet.Spreadsheet
import Spreadsheet.Parser

emptySpreadsheet :: Spreadsheet
emptySpreadsheet = SS empty Nothing

-- text representation for showing
getCellText :: CellID -> Spreadsheet -> String
getCellText id ss = case lab (ss^.sheet) id of
                       Nothing        -> ""
                       Just (Str str) -> str
                       Just (Ref id' _) -> getCellText id' ss

-- user given code for cell
getCellCode :: CellID -> Spreadsheet -> String
getCellCode id ss = case lab (ss^.sheet) id of
                      Nothing -> ""
                      Just (Str str) -> str
                      Just (Ref _ str) -> str

setCellState :: CellID -> String -> Spreadsheet -> Spreadsheet
setCellState id' str' ss'
  | isLegal id' newRefs ssB' = overSH ssN $ legalSet id' cell' 
  | otherwise = ss'
  where
    legalSet id (Str "") sh
      | null oldRefs = delNode id sh
      | otherwise = case match id sh of
                      (Just (p, _, l, s), cg) -> (p, id, Str "", s) & cg
                      (Nothing,            _) -> error "node does not exist!"
    legalSet _ _ _ = ssN^.sheet
    oldRefs = suc (ss'^.sheet) id'
    ssB = overSH ss' $ delEdges (zip oldRefs $ repeat id')
    ssB' = case match id' $ ssB^.sheet of
             (Just (p, _, l, s), cg) -> set sheet ((p, id', cell', s) & cg) ssB
             (Nothing,            _) -> overSH ssB $ insNode (id', cell')
    ssN = overSH ssB' $ insEdges (zip3 (repeat id') newRefs $ repeat 1)
    newRefs = references cell'
    cell' = rep str'

getSelected :: Spreadsheet -> Maybe CellID
getSelected ss = ss^.selected

setSelected :: CellID -> Spreadsheet -> Spreadsheet
setSelected = set selected . Just

-- this needs to be generalized
isLegal :: CellID -> [CellID] -> Spreadsheet -> Bool
isLegal id []  _  = True
isLegal id [ref] ss = case sp ref id $ ss^.sheet of
                         Nothing -> True
                         Just _  -> False

-- this needs to be generalized 
references :: Cell -> [CellID]
references (Str _)    = []
references (Ref cell _) = [cell]

