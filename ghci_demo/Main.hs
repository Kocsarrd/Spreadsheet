import Control.Concurrent
import Control.Monad
import Language.Haskell.Ghcid
import System.Process
import System.Timeout

evalThread :: MVar Ghci -> MVar String -> MVar (Either String String) -> IO ()
evalThread ghciR commandR resultR = forever $ do
  command <- takeMVar commandR
  ghci <- readMVar ghciR
  mResult <- timeout 200000 $ exec ghci command
  case mResult of
    Nothing -> do
      let handle = process ghci
      mPid <- getPid handle
      case mPid of
        Nothing -> putStrLn "no pid is here"
        Just pid -> do
          childPid <- init <$> readProcess "pgrep" ["-P", show pid] ""
          putMVar resultR $ Left childPid
    Just result -> putMVar resultR $ Right $ show result
  putStrLn "itt vok"

main :: IO ()
main = do
  ghciR <- createGhci >>= newMVar
  commandR <- newEmptyMVar
  resultR <- newEmptyMVar
  forkIO $ evalThread ghciR commandR resultR
  forM_ [1,2,3] $ const $ getLine >>= execCommand ghciR commandR resultR
  
  
execCommand :: MVar Ghci -> MVar String -> MVar (Either String String) -> String -> IO ()
execCommand ghciR commandR resultR command = do
  putMVar commandR command
  mResult <- takeMVar resultR
  case mResult of
    Right result -> putStrLn result
    Left pid -> do
      callProcess "kill" ["-SIGKILL", pid]
      createGhci >>= swapMVar ghciR >> pure ()
      

createGhci :: IO Ghci
createGhci = do
  ghci <- fst <$> startGhci "ghci" (Just ".") (\_ _ -> pure ())
  pure ghci
