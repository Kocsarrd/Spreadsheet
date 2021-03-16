module GUI.RunApp (appMain) where

import Control.Concurrent
import Control.Monad.Reader
import Graphics.UI.Gtk
import Language.Haskell.Ghcid

import GUI.CreateEnv
import GUI.Setup
import GUI.Types
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
  lift $ do
    windowMaximize mainW
    widgetShowAll mainW
    onDestroy mainW $ do
      mainQuit
      readMVar ghci' >>= stopGhci
      readMVar configR >>= saveModuleConfig
    mainGUI

