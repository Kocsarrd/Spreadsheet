module App.RunApp (appMain) where

import Control.Concurrent
import Control.Monad.Reader
import Graphics.UI.Gtk

import Eval.Ghci (loadModules)
import App.CreateEnv
import App.Setup
import App.Types

appMain :: IO ()
appMain = do
  env <- createEnv
  runReaderT runApp env

runApp :: App ()
runApp = do
  setupGui
  mainW <- mainWindow <$> asks gui
  ghci' <- eGhci <$> asks evalControl
  configR <- eConfig <$> asks evalControl
  withReaderT evalControl loadModules
  lift $ do
    windowMaximize mainW
    widgetShowAll mainW
    mainGUI

