{-# LANGUAGE ViewPatterns #-}

module Spreadsheet.Interface
  (
  emptySpreadsheet,
  getCellText, getCellCode, setCellState,
  cacheCell,
  getSelected, setSelected,
  getLogMessage
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query
import Data.Maybe (fromJust, isNothing)
import Data.Ratio.Rounding
import Lens.Micro.Platform hiding ((&))

import System.IO.Unsafe

import Spreadsheet.Types
import Spreadsheet.Parser

--------------------------------------------------------
-- for setting the cell state, before evaluation happens
--------------------------------------------------------

-- this is not optimal by any means
-- log messages don't handle parse errors
setCellState :: CellID -> String -> Spreadsheet -> Spreadsheet
setCellState id' str' ss'
  | isLegal id' newRefs ssB' = set logMessage (Just $ "update successful: " ++ toCellName id') $ overSH ssN $ legalSet id' cell' 
  | otherwise = set logMessage (Just $ "cyclic reference - update failed: " ++ toCellName id') $
                  overSH ss' $ cyclicErrorSet id'
  where
    legalSet :: CellID -> Cell -> Gr Cell Int -> Gr Cell Int
    legalSet id (Val (Str "")) sh
      | null oldRefs = delNode id sh
      | otherwise = lookupNodeThen id (changeNodeLab $ Val $ Str "") (error "node does not exist!") sh
    legalSet _ _ _ = ssN^.sheet
    oldRefs = pre (ss'^.sheet) id'
    ssB = overSH ss' $ delEdges (zip oldRefs $ repeat id')
    ssB' = overSH ssB $ lookupNodeThen id' (changeNodeLab cell') (const $ insNode (id',cell') $ ssB^.sheet) 
    ssN' = overSH ssB' (\sh -> foldr go sh newRefs)
    go r sh = lookupNodeThen r (const sh) (const $ insNode (r, Val (Str "")) sh) sh
    ssN = overSH ssN' $ insEdges (zip3 newRefs (repeat id') $ repeat 1)
    newRefs = references cell'
    cell' = rep str'
    cyclicErrorSet :: CellID -> Gr Cell Int -> Gr Cell Int
    cyclicErrorSet id sh = lookupNodeThen id (changeNodeLab cyclicRefError) (const $ insNode (id,cyclicRefError) sh) sh
    cyclicRefError = For $ Formula str' (Left FCycleRefError) Nothing 

------------------------------
-- for caching evaluated cells
------------------------------

-- if result is error, modify formula cache to corresponding error
-- if not, parse the value
-- this will contain a bug (if result string starts with '=')
-- (rep call should be replaced with a repCell' or smth)
cacheCell :: CellID -> Either EvalError String -> Spreadsheet -> Spreadsheet
cacheCell id result ss = overSH ss $ lookupNodeThen id
                               (changeNodeLabBy $ over (cellF.cache) $ const $ readResult result)
                               (error "node does not exist")
  where
    readResult (Left err) = Left $ convErr err
    readResult (Right ok) = Right $ fromJust $ rep ok ^? cellV 
    convErr (EGhciError _) = FGhciError
    convErr ETimeoutError = FTimeoutError


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


emptySpreadsheet :: Spreadsheet
emptySpreadsheet = SS empty Nothing Nothing

-- text representation for showing
-- pattern matches on Cell
-- number of shown decimals is hardcoded!
-- here I could use show instances, but the derived instance is better for debugging
getCellText :: CellID -> Spreadsheet -> String
getCellText id ss = case lab (ss^.sheet) id of
                       Nothing        -> ""
                       Just (For for) -> either show showCell' $ for^.cache
                       Just (Val cell') -> showCell' cell'
                       
-- user given code for cell
-- pattern matches on Cell
getCellCode :: CellID -> Spreadsheet -> String
getCellCode id ss = case lab (ss^.sheet) id of
                      Nothing -> ""
                      Just (For for) -> for^.code
                      Just (Val cell') -> showCell' cell'
                      
showCell' :: Cell' -> String
showCell' (Str str) = str
showCell' (Number num) = show num

-- useful helper functions
lookupNodeThen :: DynGraph gr => Node
               -> (Decomp gr a b -> gr a b)
               -> (Decomp gr a b -> gr a b)
               ->  gr a b -> gr a b
lookupNodeThen node ifFound ifNot gr = case match node gr of
  dc@(Just c, _) -> ifFound dc
  dc@_           -> ifNot dc

-- to call in lookupNodeThen' first case
changeNodeLabBy :: DynGraph gr => (a -> a) -> Decomp gr a b -> gr a b
changeNodeLabBy f (Just c, cg) = over _3 f c & cg

changeNodeLab :: DynGraph gr => a -> Decomp gr a b -> gr a b
changeNodeLab = changeNodeLabBy . const 


