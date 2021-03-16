{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module GUI.Types where

import Control.Concurrent (MVar)
import Data.IORef (IORef)
import Data.Serialize (Serialize)
import GHC.Generics
import Graphics.UI.Gtk (Button, Entry, Table, TextBuffer, ScrolledWindow, Window)
import Language.Haskell.Ghcid (Ghci)

import Spreadsheet.Types (Spreadsheet)

-- hardcode alert
sizeX, sizeY :: Int
sizeX = 15
sizeY = 19

-- types for global state
data Menubar = Menubar { saveButton :: Button
                       , loadButton :: Button
                       , modulesButton :: Button
                       } deriving Eq

data Gui = Gui { mainWindow :: Window
               , logWindow  :: ScrolledWindow
               , log        :: TextBuffer
               , table      :: Table
               , entryKeys  :: [(Entry,(Int,Int))]
               , editor     :: Entry
               , menu       :: Menubar
               } deriving Eq

data EvalConfig = EvalConfig [String]
  deriving (Eq, Generic)

instance Serialize EvalConfig where

data EvalControl = EvalControl { eGhci    :: MVar Ghci
                               , eCommand :: MVar String
                               , eResult  :: MVar (Either String [String])
                               , eConfig  :: MVar EvalConfig 
                               } deriving Eq

data Env = Env { evalControl  :: EvalControl
               , gui       :: Gui
               , state     :: IORef Spreadsheet
               } deriving Eq
