module Eval.EvalMain (evalMain) where

import Control.Concurrent
import Control.Monad
import Language.Haskell.Ghcid
import System.Process
import System.Timeout

import GUI.Types

-- thread to evaluate expressions in ghci
evalMain :: EvalControl -> IO ()
evalMain (EvalControl ghciR commandR resultR _) = forever $ do
  command <- takeMVar commandR
  ghci <- readMVar ghciR
  mResult <- timeout 1000000 $ exec ghci command
  case mResult of
    Nothing -> do
      let handle = process ghci
      mPid <- getPid handle
      case mPid of
        -- this should not happen in theory
        Nothing -> putStrLn "no pid is here"
        Just pid -> do
          childPid <- init <$> readProcess "pgrep" ["-P", show pid] ""
          putMVar resultR $ Left childPid
    Just result -> putMVar resultR $ Right $ result
  -- debug
  --putStrLn "Fazekas Sándor"

