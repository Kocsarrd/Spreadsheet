module Spreadsheet.CodeGeneration where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Spreadsheet.Types
import Spreadsheet.Parser

-- generate code for a single cell
cell :: Cell -> String
cell (Val (Str str)) = '"' : str ++ "\""
cell (Val (Number num)) = trimmed
  where
    trimmed = if decimal == "0" then integer else numS
    [integer,decimal] = splitOn "." numS
    numS = show $ fromRational num

cell (For (Formula _ (Just val) _)) = cell $ Val val
cell (For (Formula _ _ pieces)) = foldr go "" pieces
  where
    go (Code code) acc = code ++ acc
    go (Refs [id]) acc = 'v' : show id ++ acc
    go (Refs ids)  acc = '[' : (intercalate "," $ map (('v':) . show) ids) ++ "]"

