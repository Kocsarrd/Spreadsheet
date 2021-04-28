{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Types where

import Control.Concurrent (MVar)
import Data.IORef (IORef)
import Data.Serialize (Serialize)
import GHC.Generics
import Graphics.UI.Gtk (Button, Entry, Table, TextBuffer, ScrolledWindow, Window)
import Language.Haskell.Ghcid (Ghci)
import Lens.Micro.Platform

import Spreadsheet.Types (Spreadsheet)

-- hardcode alert
sizeX, sizeY :: Int
sizeX = 25
sizeY = 100

-- types for global state

data Menubar = Menubar { newButton :: Button
                       , saveButton :: Button
                       , loadButton :: Button
                       , modulesButton :: Button
                       , pathsButton :: Button
                       } deriving Eq

data Gui = Gui { mainWindow  :: Window
               , logWindow   :: ScrolledWindow
               , log         :: TextBuffer
               , table       :: Table
               , entryKeys   :: [(Entry,(Int,Int))]
               , editor      :: Entry
               , commandLine :: Entry
               , menu        :: Menubar
               } deriving Eq

data EvalConfig = EvalConfig { modules :: [String]
                             , paths :: [String]
                             }
  deriving (Eq, Generic)

ecSetModules ms (EvalConfig _ ps) = EvalConfig ms ps
ecSetPaths ps (EvalConfig ms _) = EvalConfig ms ps

instance Serialize EvalConfig where

data EvalControl = EvalControl { eGhci    :: MVar Ghci
                               , eCommand :: MVar String
                               , eResult  :: MVar (Either String [String])
                               , eConfig  :: MVar EvalConfig 
                               } deriving Eq

data SaveStatus = Saved | Modified
  deriving Eq

data File = File FilePath SaveStatus
  deriving Eq

setStatus st (File fp _) = File fp st  

data Env = Env { evalControl  :: EvalControl
               , gui          :: Gui
               , state        :: IORef Spreadsheet
               , file         :: IORef (Maybe File)
               } deriving Eq
