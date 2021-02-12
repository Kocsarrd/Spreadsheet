{-# LANGUAGE FlexibleInstances #-}

module Spreadsheet.Spreadsheet (
  Cell(..), CellID,
  Spreadsheet(..)
  ) where

import Data.Graph.Inductive.PatriciaTree

type CellID = Int

data Cell = Str String | Ref CellID
  deriving (Eq, Show)

type Spreadsheet = Gr Cell Int
      
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
