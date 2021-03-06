module App.Setup (setupGui) where

import Control.Monad.Reader

import App.Setup.CommandLine
import App.Setup.Editor
import App.Setup.MainWindow
import App.Setup.Menubar
import App.Setup.Table
import App.Types

-- connects the GUI with the logic
setupGui :: App ()
setupGui = do
  setupEditor
  setupCommandLine
  setupMenubar
  setupTable
  setupMainWindow

