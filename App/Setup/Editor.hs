module App.Setup.Editor (setupEditor) where

import Control.Monad.Reader
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import App.Setup.Global

setupEditor :: App ()
setupEditor = do
  ed <- asksGui editor
  env <- ask
  lift $ onFocusOut ed (\e -> runReaderT editorLosesFocus env)
  lift $ onEntryActivate ed $ void $ runReaderT editorLosesFocus env
  lift $ onFocusIn ed (\e -> runReaderT editorGetsFocus env)
  pure ()

editorGetsFocus :: App Bool
editorGetsFocus = do
  ss <- askState >>= liftIO . readIORef
  ed <- asksGui editor
  lift $ do
    case getSelected ss of
      Nothing -> entrySetText ed ""
      Just key -> entrySetText ed (getCellCode key ss)
  pure False

editorLosesFocus :: App Bool
editorLosesFocus = do
  ssR <- askState
  ed <- asksGui editor
  ss <- lift $ readIORef ssR
  newText <- lift $ entryGetText ed 
  case getSelected ss of
    Nothing -> pure ()
    Just key -> lift (unless (newText == getCellText key ss) $
                      modifyIORef' ssR $ setCellState key newText) >> evalAndSet key
  updateView
  pure False
  
