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

import GUI.Types
import Spreadsheet.Types

execGhciCommand :: (String,[CellID]) -> ReaderT EvalControl IO (Either EvalError [(CellID,String)])
execGhciCommand (code,ids) = do
  result <- execG code
  pure $ either Left (getResult ids) result

execGhciQuery :: String -> ReaderT EvalControl IO (Either EvalError String)
execGhciQuery str = do
  result <- execGhciCommand (str, [0])
  pure $ snd . head <$> result

execG :: String -> ReaderT EvalControl IO (Either EvalError [String])
execG command = do
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
loadModules :: ReaderT EvalControl IO ()
loadModules = do
  EvalControl _ _ _ configR <- ask
  EvalConfig ms ps <- lift $ readMVar configR
  let pathCommands = map ((++) ":set -i") ps
  let loadCommands = map ((++) "import ") ms ++ map ((++) ":l ") ms 
  mapM_ execG $ pathCommands ++ loadCommands
  
createGhci :: IO Ghci
createGhci = do
  ghci <- fst <$> startGhci "ghci" (Just ".") (\_ _ -> pure ())
  pure ghci

-- converts result of evaluation to list of results
getResult :: [CellID] -> [String] -> Either EvalError [(CellID, String)]
getResult ids [str] = case parse resultP "" str of
                        Left _ -> error "result parser error"
                        Right res -> Right $ zip ids res
getResult _ xs = Left $ EGhciError xs

resultP :: Parser [String]
resultP = between (char '(') (char ')') p <|> (pure <$> many anyChar)
  where
    p = many (noneOf "),") `sepBy` char ','
