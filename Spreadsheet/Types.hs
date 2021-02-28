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

data Cell' = Str String | Number Double
  deriving (Eq, Show, Generic)

-- I need to come up with a better name lol
data ForPiece = Code String | Refs [CellID]
  deriving (Eq, Show, Generic)

data FormulaError = NoParse
                  | CycleRefError
                  | NoCache
                  | ListTypeError
                  | GHCIError
                  | TimeoutError
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

{-
All allowed states of a Formula:

           pattern                     |                                            meaning
---------------------------------------|--------------------------------------------------------------------------------------------------------
Formula _ (Left NoParse)       Nothing | the formula could not be parsed
Formula _ (Left CycleRefError) Nothing | parse was successful, but there is a reference cycle
Formula _ (Left NoCache)       Just _  | parse was successful, references are valid, but the formula is not evaluated yet
Formula _ (Left ListTypeError) Just _  | the formula cannot be evaluated, because there is at least one inhomogenous reference list
Formula _ (Left GHCIError)     Just _  | the formula cannot be evaluated for other reasons (e.g. syntax error, compile error, runtime exception)
Formula _ (Left TimeoutError)  Just _  | the evaluation timed out, caused most likely by an infinite loop
Formula _ (Right cell')        Just _  | the evaluation was successful, the result is cell'
------------------------------------------------------------------------------------------------------------------------------------------------
-}

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
