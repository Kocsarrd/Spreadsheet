{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Spreadsheet.Spreadsheet (
  Cell(..), CellID,
  Spreadsheet(..),
  sheet, selected,
  overSH
  ) where

import GHC.Generics
import Data.Graph.Inductive.PatriciaTree
import Data.Serialize (Serialize)
import Lens.Micro.Platform

type CellID = Int

data Cell = Str String | Ref CellID String
  deriving (Eq, Show, Generic)

data Spreadsheet = SS { _sheet :: Gr Cell Int
                      , _selected :: Maybe CellID}
  deriving(Eq, Show, Generic)
                   
makeLenses ''Spreadsheet
overSH ss f = over sheet f ss

instance Serialize Cell where

instance (Serialize a, Serialize b) => Serialize (Gr a b) where

instance Serialize Spreadsheet where
  
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
