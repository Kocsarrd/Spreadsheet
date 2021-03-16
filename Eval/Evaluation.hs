module Eval.Evaluation where

import Control.Concurrent
import Language.Haskell.Ghcid
import System.Process

import GUI.Types
import Spreadsheet.Types

execCommand :: EvalControl -> String -> IO (Either EvalError [String])
execCommand (EvalControl ghciR commandR resultR) command = do
  putMVar commandR command
  mResult <- takeMVar resultR
  case mResult of
    Right result -> pure $ Right result
    Left pid -> do
      callProcess "kill" ["-SIGKILL", pid]
      createGhci >>= swapMVar ghciR >> pure (Left ETimeoutError)
      

createGhci :: IO Ghci
createGhci = do
  ghci <- fst <$> startGhci "ghci" (Just ".") (\_ _ -> pure ())
  pure ghci
