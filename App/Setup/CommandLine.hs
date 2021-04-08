module App.Setup.CommandLine (setupCommandLine) where

import Control.Monad.Reader
import Data.List (intercalate)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import App.Setup.Global
import App.Types
import Eval.CommandLine
import Eval.Ghci
import Spreadsheet.Types

setupCommandLine :: ReaderT Env IO ()
setupCommandLine = do
  cl <- asksGui commandLine
  env <- ask
  void $ lift $ onEntryActivate cl $ runReaderT commandLineActivated env

-- need update feature
-- this is a bit ugly, exec should give back EGhciError
commandLineActivated :: ReaderT Env IO ()
commandLineActivated = do
  cl <- asksGui commandLine
  command <- lift $ entryGetText cl
  lift $ entrySetText cl ""
  case parseCommand command of
    Just (ClGhci str) -> do
      result <- withReaderT evalControl $ execGhciQuery str
      case result of
        Left ETimeoutError -> logAppendText $ "query timed out: " ++ command
        Right res -> logAppendText res
        Left (EGhciError errs) -> logAppendText $ intercalate "\n" errs
    _ -> logAppendText $ "unknown command: " ++ command
    
