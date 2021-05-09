module App.RunApp (appMain) where

import Control.Concurrent
import Control.Monad.Reader
import Graphics.UI.Gtk
import Language.Haskell.Ghcid

import Eval.Ghci (loadModules)
import App.CreateEnv
import App.Setup
import App.Types
import Persistence

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
    onDelete mainW $ \e -> do
      dialog <- dialogNew
      windowSetTitle dialog "Are you sure? Any unsaved work will be lost!"
      dialogAddButton dialog "Yes" ResponseYes
      dialogAddButton dialog "No" ResponseNo
      ((==) ResponseNo) <$> (dialogRun dialog <* widgetDestroy dialog)
    onDestroy mainW $ do
      mainQuit
      readMVar ghci' >>= stopGhci
      readMVar configR >>= saveModuleConfig
    mainGUI

