module App.RunApp (appMain) where

import Control.Concurrent
import Control.Monad.Reader
import Graphics.UI.Gtk
import Language.Haskell.Ghcid

import Eval.Ghci (loadModules)
import App.CreateEnv
import App.Setup.Setup
import App.Types
import Persistence

appMain :: IO ()
appMain = do
  env <- createEnv
  runReaderT runApp env

runApp :: ReaderT Env IO ()
runApp = do
  setupGui
  mainW <- mainWindow <$> asks gui
  ghci' <- eGhci <$> asks evalControl
  configR <- eConfig <$> asks evalControl
  withReaderT evalControl loadModules
  lift $ do
    windowMaximize mainW
    widgetShowAll mainW
    onDestroy mainW $ do
      mainQuit
      readMVar ghci' >>= stopGhci
      readMVar configR >>= saveModuleConfig
    mainGUI

