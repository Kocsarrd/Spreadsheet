module App.Setup.Table (setupTable) where

import Control.Monad.Reader
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import App.Setup.Global
import App.Types
import Spreadsheet.Interface

----------------------------------
-- table showing the sheet content
----------------------------------

setupTable :: ReaderT Env IO ()
setupTable = do
  ek <- asksGui entryKeys
  env <- ask
  lift $ forM_ ek $ \(entry,mn) -> do
    onFocusIn entry $ (\e -> runReaderT (cellGetsFocus mn e) env)
    onFocusOut entry $ (\e -> runReaderT (cellLosesFocus entry mn) env)
    void $ onEntryActivate entry $ void $ runReaderT (cellLosesFocus entry mn) env

cellGetsFocus :: (Int, Int) -> Event -> ReaderT Env IO Bool
cellGetsFocus (k1,k2) _ = do
  ssR <- askState
  ed <- asksGui editor
  lift $ do
    modifyIORef' ssR $ setSelected (fromEnum (k2,k1))
    ss <- readIORef ssR
    entrySetText ed (getCellCode (fromEnum (k2,k1)) ss)
    --debug:
    --putStrLn $ "on get: " ++ show ss
    pure False

cellLosesFocus :: Entry -> (Int,Int) -> ReaderT Env IO Bool
cellLosesFocus entry (k1,k2)  = do
  ssR <- askState
  lift $ do
    ss <- readIORef ssR
    entryText <- entryGetText entry
    unless (entryText == getCellText (fromEnum (k2,k1)) ss) $
      modifyIORef' ssR $ setCellState (fromEnum (k2,k1)) entryText
  evalAndSet (fromEnum (k2,k1))
  updateView
  pure False

