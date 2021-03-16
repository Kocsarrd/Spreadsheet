module Eval.Evaluation where

import Control.Concurrent
import Control.Monad.Reader
import Language.Haskell.Ghcid
import System.Process

import GUI.Types
import Spreadsheet.Types

execGhciCommand :: String -> ReaderT EvalControl IO (Either EvalError [String])
execGhciCommand command = do
  EvalControl ghciR commandR resultR configR <- ask
  lift $ putMVar commandR command
  mResult <- lift $ takeMVar resultR
  case mResult of
    Right result -> pure $ Right result
    Left pid -> do
      lift $ callProcess "kill" ["-SIGKILL", pid]
      lift $ createGhci >>= swapMVar ghciR
      loadModules
      pure (Left ETimeoutError)
      

-- no response if module could not be loaded
loadModules :: ReaderT EvalControl IO ()
loadModules = do
  EvalControl _ _ _ configR <- ask
  EvalConfig xs <- lift $ readMVar configR
  let loadCommands = map ((++) "import ") xs
  mapM_ execGhciCommand loadCommands
  
createGhci :: IO Ghci
createGhci = do
  ghci <- fst <$> startGhci "ghci" (Just ".") (\_ _ -> pure ())
  pure ghci
