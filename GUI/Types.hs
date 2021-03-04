module GUI.Types where

import Data.IORef (IORef)
import Graphics.UI.Gtk (Button, Entry, Table, TextBuffer, Window)
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
               , log        :: TextBuffer
               , table      :: Table
               , entryKeys  :: [(Entry,(Int,Int))]
               , editor     :: Entry
               , menu       :: Menubar
               } deriving Eq

data Env = Env { ghci  :: Ghci
               , gui   :: Gui
               , state :: IORef Spreadsheet
               } deriving Eq
