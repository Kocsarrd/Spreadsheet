module App.Setup.CommandLine (setupCommandLine) where

import Control.Monad.Reader
import Data.IORef
import Data.List (intercalate)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import App.Setup.Global
import App.Types
import Eval.CommandLine
import Eval.Ghci
import Spreadsheet.Interface
import Spreadsheet.Types

setupCommandLine :: ReaderT Env IO ()
setupCommandLine = do
  cl <- asksGui commandLine
  env <- ask
  void $ lift $ onEntryActivate cl $ runReaderT commandLineActivated env


-- this is a bit ugly, exec should give back EGhciError
-- after cp and mv, evaluation does not happen yet
commandLineActivated :: ReaderT Env IO ()
commandLineActivated = do
  cl <- asksGui commandLine
  ssR <- askState
  ss <- liftIO $ readIORef ssR
  command <- lift $ entryGetText cl
  lift $ entrySetText cl ""
  case parseCommand command of
    Just (ClGhci str) -> do
      result <- withReaderT evalControl $ execGhciQuery str
      case result of
        Left ETimeoutError -> logAppendText $ "query timed out: " ++ command
        Right res -> logAppendText res
        Left (EGhciError errs) -> logAppendText $ intercalate "\n" errs
    Just (ClMv ps) -> do
      let codes = map (flip getCellCode ss . fst) ps
      lift $ mapM_ (modifyIORef' ssR . uncurry setCellState) $ zip (map snd ps) codes
      lift $ mapM_ (modifyIORef' ssR . flip setCellState "") $ map fst ps
      mapM_ evalAndSet $ map fst ps ++ map snd ps
      updateView
    Just (ClCp ps) -> do
      let codes = map (flip getCellCode ss . fst) ps
      lift $ mapM_ (modifyIORef' ssR . uncurry setCellState) $ zip (map snd ps) codes
      mapM_ evalAndSet $ map snd ps
      updateView
    _ -> logAppendText $ "unknown command: " ++ command
    
