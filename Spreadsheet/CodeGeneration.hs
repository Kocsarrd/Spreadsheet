module Spreadsheet.CodeGeneration (generateCode, GenError(..)) where

import Data.Graph.Inductive (lab,pre)
import Data.Graph.Inductive.PatriciaTree (Gr) 
import Data.Graph.Inductive.Query (bfs)
import Data.List (intercalate, nub, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Lens.Micro ((^.))
import Spreadsheet.Types
import System.IO.Unsafe

-- generate code from given data
-- list type check is not yet handled
-- if id has parse error, GenMissingDep is not informative
-- evaluation can also be called for an id not in the graph (this is not nice imo)
generateCode :: Spreadsheet -> CellID -> Either GenError (String,[CellID])
generateCode sh id = maybe (Left GenMissingDep) (Right . codeG) $ depList (sh^.sheet) id
  
-- generate code for a list of cells
-- it is assumed that a cell only depends on cells that precede it in the list
codeG :: ([(Cell,CellID)],[(Cell,CellID)]) -> (String,[CellID])
codeG (xs,ys) = (foldr go1 "" xs ++ foldr go2 "" ys ++ final , map snd ys)
  where
    go1 (cell, id) acc = ("let " ++ 'v' : show id ++ " = " ++ cacheG cell ++ " in ") ++ acc
    go2 (cell,id) acc = ("let " ++ 'v' : show id ++ " = " ++ cellG cell ++ " in ") ++ acc
    final = '(' : (intercalate "," $ map (('v':) . show . snd) ys) ++ ")"
  
-- generate code for cells that do not depend on the changed cell
cacheG :: Cell -> String
cacheG (Val (Str str)) = '"' : str ++ "\""
cacheG (Val (Number num)) = trimmed
  where
    trimmed = if decimal == "0" then integer else numS
    [integer,decimal] = splitOn "." numS
    numS = show num
cacheG (For (Formula _ (Right val) _)) = cacheG $ Val val
cacheG _ = error "cacheG: cell cache was empty"

-- generate code for cells that depend on the changed cell
cellG :: Cell -> String
cellG (For (Formula _ _ (Just pieces))) = foldr go "" pieces
  where
    go (Code code) acc = code ++ acc
    go (Refs [id]) acc = 'v' : show id ++ acc
    go (Refs ids)  acc = '[' : (intercalate "," $ map (('v':) . show) ids) ++ "]"
cellG (For _) = error "cellG: cell was not ready"
cellG _ = error "cellG: cell was not a formula"

-- collect all cells that depend on or are dependencies of a given cell
-- a cell only depends on cells that precede it in the resulting list
-- if a dependency's value is not ready, Nothing is returned
-- first list contains outer dependencies (can be read from cache)
-- second list contains cell that depend on given id (need to be reevaluated)
depList :: Gr Cell Int -> CellID -> Maybe ([(Cell,CellID)],[(Cell,CellID)])
depList sh id = if ok then Just (lOuterDeps, lDependOnId) else Nothing     
  where
    ok = not (null dependOnId') && all cached lOuterDeps && all ready lDependOnId 
    lOuterDeps = map (\i -> (fromJust (lab sh i), i)) outerDeps
    lDependOnId = map (\i -> (fromJust (lab sh i), i)) dependOnId  
    outerDeps' = nub (dependOnId >>= pre sh) \\ dependOnId
    dependOnId' = bfs id sh
    (outerDeps,dependOnId) = case lab sh id of
                               Just (Val _) -> (id : outerDeps', tail dependOnId')
                               Just (For _) -> (outerDeps',dependOnId')
                               _ -> ([],[])
cached :: (Cell, CellID) -> Bool
cached (Val _ ,_) = True
cached (For (Formula _ (Right _) _),_) = True
cached _ = False

ready :: (Cell, CellID) -> Bool
ready ((For (Formula _ (Right val) _)),_) = True
ready ((For (Formula _ _ (Just pieces))),_) = True
ready _ = False
