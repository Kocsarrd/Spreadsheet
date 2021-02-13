{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Spreadsheet.Spreadsheet (
  Cell(..), CellID,
  Spreadsheet(..),
  sheet, selected,
  overSH
  ) where

import Data.Graph.Inductive.PatriciaTree
import Lens.Micro.Platform

type CellID = Int

data Cell = Str String | Ref CellID String
  deriving (Eq, Show)

data Spreadsheet = SS { _sheet :: Gr Cell Int
                      , _selected :: Maybe CellID}
  deriving(Show)
                   
makeLenses ''Spreadsheet
overSH ss f = over sheet f ss
  
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
