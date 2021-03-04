module GUI.RunApp (appMain) where

import Control.Monad.Reader
import Graphics.UI.Gtk
import Language.Haskell.Ghcid

import GUI.CreateEnv
import GUI.Setup
import GUI.Types

appMain :: IO ()
appMain = do
  env <- createEnv
  runReaderT runApp env

-- onDestroy may need to be rewritten, if I want to change ghci sessions
runApp :: ReaderT Env IO ()
runApp = do
  setupGui
  mainW <- mainWindow <$> asks gui
  ghci' <- asks ghci
  lift $ do
    windowMaximize mainW
    widgetShowAll mainW
    onDestroy mainW $ mainQuit >> stopGhci ghci'
    mainGUI

