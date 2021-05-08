module Eval.Ghci
  (
  execGhciCommand,
  execGhciQuery,
  loadModules
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Language.Haskell.Ghcid
import System.Process
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import App.Types
import Spreadsheet.Types

type Eval a = ReaderT EvalControl IO a

execGhciCommand :: String -> Eval (Either EvalError String)
execGhciCommand code = do
  result <- execG code
  pure $ either Left getResult result

-- this is deprecated, only kept for historic reasons
execGhciQuery :: String -> Eval (Either EvalError String)
execGhciQuery = execGhciCommand

execG :: String -> ReaderT EvalControl IO (Either EvalError [String])
execG command = do
  --debug
  --lift $ putStrLn command
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
-- sets paths and loads modules
-- this is currently very ugly
loadModules :: Eval ()
loadModules = do
  EvalControl _ _ _ configR <- ask
  EvalConfig ms ps <- lift $ readMVar configR
  let pathCommands = map ((++) ":set -i") ps
  let loadCommands = map ((++) "import ") ms ++ map ((++) ":l ") ms
  execG ":l"
  mapM_ execG $ pathCommands ++ loadCommands
  -- needed for (€) operator to work
  void $ execGhciCommand ":l Empty.hs"
  
createGhci :: IO Ghci
createGhci = do
  ghci <- fst <$> startGhci "ghci" (Just ".") (\_ _ -> pure ())
  pure ghci

-- converts result of evaluation to list of results
getResult :: [String] -> Either EvalError String
getResult [] = Right ""
getResult [str] = Right str
getResult xs = Left $ EGhciError xs
