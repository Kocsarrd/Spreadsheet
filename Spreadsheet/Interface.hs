module Spreadsheet.Interface
  (
  emptySpreadsheet,
  getCellText, getCellCode, setCellState,
  getSelected, setSelected,
  getLogMessage
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query
import Data.Maybe (isNothing)
import Data.Ratio.Rounding
import Lens.Micro.Platform hiding ((&))

import Spreadsheet.Types
import Spreadsheet.Parser

emptySpreadsheet :: Spreadsheet
emptySpreadsheet = SS empty Nothing Nothing

-- text representation for showing
-- pattern matches on Cell
-- number of shown decimals is hardcoded!
-- here I could use show instances, but the derived instance is better for debugging
getCellText :: CellID -> Spreadsheet -> String
getCellText id ss = case lab (ss^.sheet) id of
                       Nothing        -> ""
                       Just (For for) -> either show showCell' $ cache for 
                       Just (Val cell') -> showCell' cell'
                       
-- user given code for cell
-- pattern matches on Cell
getCellCode :: CellID -> Spreadsheet -> String
getCellCode id ss = case lab (ss^.sheet) id of
                      Nothing -> ""
                      Just (For for) -> code for
                      Just (Val cell') -> showCell' cell'
                      
showCell' :: Cell' -> String
showCell' (Str str) = str
showCell' (Number num) = show $ fromRational $ dpRound 3 num
                        
setCellState :: CellID -> String -> Spreadsheet -> Spreadsheet
setCellState id' str' ss'
  | isLegal id' newRefs ssB' = set logMessage (Just $ "update successful: " ++ toCellName id') $ overSH ssN $ legalSet id' cell' 
  | otherwise = set logMessage (Just $ "cyclic reference - update failed: " ++ toCellName id') $
                  overSH ss' $ cyclicErrorSet id'
  where
    legalSet :: CellID -> Cell -> Gr Cell Int -> Gr Cell Int
    legalSet id (Val (Str "")) sh
      | null oldRefs = delNode id sh
      | otherwise = case match id sh of
                      (Just (p, _, l, s), cg) -> (p, id, Val (Str ""), s) & cg
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
    cyclicErrorSet :: CellID -> Gr Cell Int -> Gr Cell Int
    cyclicErrorSet id sh =  case match id $ sh of
                              (Just (p, _, l, s), cg) -> (p, id, cyclicRefError, s) & cg
                              (Nothing,            _) -> insNode (id,cyclicRefError) sh
    cyclicRefError = For $ Formula str' (Left CycleRefError) Nothing 

getSelected :: Spreadsheet -> Maybe CellID
getSelected ss = ss^.selected

setSelected :: CellID -> Spreadsheet -> Spreadsheet
setSelected = set selected . Just

getLogMessage :: Spreadsheet -> String
getLogMessage ss = case ss^.logMessage of
                     Just str -> str
                     Nothing  -> ""

isLegal :: CellID -> [CellID] -> Spreadsheet -> Bool
isLegal id refs ss = all (\ref -> isNothing $ sp ref id $ ss^.sheet) refs

toCellName :: CellID -> String
toCellName id = toEnum (r+65) : show c
  where
    (r,c) = toEnum id :: (Int,Int)

-- pattern matches on Cell
-- if value is nothing (there was no parse), this returns []
references :: Cell -> [CellID]
references (For (Formula _ _ (Just val))) = foldr folder [] $ val
  where
    folder (Code _) refs = refs
    folder (Refs r) refs = r ++ refs
references _ = []
