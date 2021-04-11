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
    onFocusOut entry $ (\e -> runReaderT (cellLosesFocus entry mn e) env)

cellGetsFocus :: (Int, Int) -> Event -> ReaderT Env IO Bool
cellGetsFocus key _ = do
  ssR <- askState
  ed <- asksGui editor
  lift $ do
    modifyIORef' ssR $ setSelected (fromEnum key)
    ss <- readIORef ssR
    entrySetText ed (getCellCode (fromEnum key) ss)
    --debug:
    --putStrLn $ "on get: " ++ show ss
    pure False

cellLosesFocus :: Entry -> (Int,Int) -> Event -> ReaderT Env IO Bool
cellLosesFocus entry key _ = do
  ssR <- askState
  lift $ do
    ss <- readIORef ssR
    entryText <- entryGetText entry
    unless (entryText == getCellText (fromEnum key) ss) $
      modifyIORef' ssR $ setCellState (fromEnum key) entryText
  evalAndSet (fromEnum key)
  updateView
  pure False

