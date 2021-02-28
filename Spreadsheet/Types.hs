{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Spreadsheet.Types (
  CellID,Cell'(..), ForPiece(..), FormulaError(..), Formula(..), Cell(..),
  Spreadsheet(..),
  sheet, selected, logMessage,
  overSH
  ) where

import GHC.Generics
import Data.Graph.Inductive.PatriciaTree
import Data.Serialize (Serialize)
import Lens.Micro.Platform

type CellID = Int

data Cell' = Str String | Number Rational
  deriving (Eq, Show, Generic)

-- I need to come up with a better name lol
data ForPiece = Code String | Refs [CellID]
  deriving (Eq, Show, Generic)

data FormulaError = NoCache
                  | NoParse
                  | ListTypeError
                  | GHCIError
                  | RefError
  deriving (Eq, Show, Generic)

data Formula = Formula { code :: String
                       , cache :: Either FormulaError Cell'
                       , value :: Maybe [ForPiece]
                       }
  deriving (Eq, Show, Generic)

data Cell = Val Cell' | For Formula
  deriving (Eq, Show, Generic)

data Spreadsheet = SS { _sheet :: Gr Cell Int
                      , _selected :: Maybe CellID
                      , _logMessage :: Maybe String
                      }
  deriving(Eq, Show, Generic)
                   
makeLenses ''Spreadsheet
overSH ss f = over sheet f ss

instance Serialize Cell' where

instance Serialize ForPiece where

instance Serialize FormulaError where
  
instance Serialize Formula where
  
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
