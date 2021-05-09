module App.Setup.MainWindow where

import Control.Concurrent
import Control.Monad.Reader
import Graphics.UI.Gtk
import Language.Haskell.Ghcid

import App.Setup.Global
import Eval.Ghci
import Persistence

setupMainWindow :: App ()
setupMainWindow = do
  ghci' <- eGhci <$> asks evalControl
  configR <- eConfig <$> asks evalControl
  mainW <- asksGui mainWindow
  void $ lift $ do
    onDelete mainW $ \e -> not <$> runAreYouSureDialog
    onDestroy mainW $ do
      mainQuit
      readMVar ghci' >>= stopGhci
      readMVar configR >>= saveModuleConfig
