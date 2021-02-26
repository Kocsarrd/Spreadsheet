module Spreadsheet.CodeGeneration where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Spreadsheet.Types

-- generate code for a list of cells
-- it is assumed that a cell only depends on cells that precede it in the list
generateCode :: [(Cell,CellID)] -> String
generateCode xs = foldr go "" xs ++ final
  where
    go (cell,id) acc = ("let " ++ 'v' : show id ++ " = " ++ cellG cell ++ " in ") ++ acc
    final = '(' : (intercalate "," $ map (('v':) . show . snd) xs) ++ ")"

  
-- generate code for a single cell
cellG :: Cell -> String
cellG (Val (Str str)) = '"' : str ++ "\""
cellG (Val (Number num)) = trimmed
  where
    trimmed = if decimal == "0" then integer else numS
    [integer,decimal] = splitOn "." numS
    numS = show $ fromRational num

cellG (For (Formula _ (Just val) _)) = cellG $ Val val
cellG (For (Formula _ _ pieces)) = foldr go "" pieces
  where
    go (Code code) acc = code ++ acc
    go (Refs [id]) acc = 'v' : show id ++ acc
      go (Refs ids)  acc = '[' : (intercalate "," $ map (('v':) . show) ids) ++ "]"

