module Main where

import Control.Monad.Reader (runReaderT)
import GUI.Run2

main :: IO ()
main = do
  env <- createEnv
  runReaderT runApp env
  
