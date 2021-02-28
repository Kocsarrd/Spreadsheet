module Spreadsheet.CodeGeneration (generateCode, GenError(..)) where

import Data.Graph.Inductive (lab,pre)
import Data.Graph.Inductive.PatriciaTree (Gr) 
import Data.Graph.Inductive.Query (bfs)
import Data.List (intercalate, nub, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Lens.Micro ((^.))
import Spreadsheet.Types

data GenError = GenListType | GenMissingDep
  deriving (Eq,Show)

-- generate code from given data
-- list type check is not yet handled
generateCode :: Spreadsheet -> CellID -> Either GenError String
generateCode sh id = maybe (Left GenMissingDep) (Right . codeG) $ depList (sh^.sheet) id
  
-- generate code for a list of cells
-- it is assumed that a cell only depends on cells that precede it in the list
codeG :: ([(Cell,CellID)],[(Cell,CellID)]) -> String
codeG (xs,ys) = foldr go "" (xs ++ ys) ++ final
  where
    go (cell,id) acc = ("let " ++ 'v' : show id ++ " = " ++ cellG cell ++ " in ") ++ acc
    final = '(' : (intercalate "," $ map (('v':) . show . snd) ys) ++ ")"

  
-- generate code for a single cell
cellG :: Cell -> String
cellG (Val (Str str)) = '"' : str ++ "\""
cellG (Val (Number num)) = trimmed
  where
    trimmed = if decimal == "0" then integer else numS
    [integer,decimal] = splitOn "." numS
    numS = show num

-- error is bugged, we need a few more lines
cellG (For (Formula _ (Right val) _)) = cellG $ Val val
cellG (For (Formula _ _ (Just pieces))) = foldr go "" pieces
  where
    go (Code code) acc = code ++ acc
    go (Refs [id]) acc = 'v' : show id ++ acc
    go (Refs ids)  acc = '[' : (intercalate "," $ map (('v':) . show) ids) ++ "]"
cellG _ = error "code generation should not have been called"

-- collect all cells that depend on or are dependencies of a given cell
-- a cell only depends on cells that precede it in the resulting list
-- if a dependency's value is not cached, Nothing is returned
depList :: Gr Cell Int -> CellID -> Maybe ([(Cell,CellID)],[(Cell,CellID)])
depList sh id = if ok then Just (lOuterDeps, lDependOnId) else Nothing 
  where
    ok = all cached lOuterDeps
    lOuterDeps = map (\i -> (fromJust (lab sh i), i)) outerDeps
    lDependOnId = map (\i -> (fromJust (lab sh i), i)) dependOnId  
    outerDeps = nub (dependOnId >>= pre sh) \\ dependOnId
    dependOnId = bfs id sh
    
cached :: (Cell, CellID) -> Bool
cached (Val _ ,_) = True
cached (For (Formula _ (Right _) _),_) = True
cached _ = False
