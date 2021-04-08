module App.Setup.Editor (setupEditor) where

import Control.Monad.Reader
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import App.Setup.Global
import App.Types
import Spreadsheet.Interface

setupEditor :: ReaderT Env IO ()
setupEditor = do
  ed <- asksGui editor
  env <- ask
  lift $ onFocusOut ed (\e -> runReaderT (editorLosesFocus e) env)
  lift $ onFocusIn ed (\e -> runReaderT (editorGetsFocus e) env)
  pure ()

editorGetsFocus :: Event -> ReaderT Env IO Bool
editorGetsFocus _ = do
  ss <- askState >>= liftIO . readIORef
  ed <- asksGui editor
  lift $ do
    case getSelected ss of
      Nothing -> entrySetText ed ""
      Just key -> entrySetText ed (getCellCode key ss)
  pure False

editorLosesFocus :: Event -> ReaderT Env IO Bool
editorLosesFocus e = do
  ssR <- askState
  ed <- asksGui editor
  ss <- lift $ readIORef ssR
  newText <- lift $ entryGetText ed 
  case getSelected ss of
    Nothing -> pure ()
    Just key -> lift (unless (newText == getCellText key ss) $
                         modifyIORef' ssR $ setCellState key newText) >>
                evalAndSet key
  updateView
  pure False
