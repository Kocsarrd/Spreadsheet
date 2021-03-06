module GUI.Types where

import Control.Concurrent (MVar)
import Data.IORef (IORef)
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
                       } deriving Eq

data Gui = Gui { mainWindow :: Window
               , logWindow  :: ScrolledWindow
               , log        :: TextBuffer
               , table      :: Table
               , entryKeys  :: [(Entry,(Int,Int))]
               , editor     :: Entry
               , menu       :: Menubar
               } deriving Eq

data EvalData = EvalData { eGhci    :: MVar Ghci
                         , eCommand :: MVar String
                         , eResult :: MVar (Either String String)
                         } deriving Eq

data Env = Env { evalData  :: EvalData
               , gui       :: Gui
               , state     :: IORef Spreadsheet
               } deriving Eq
