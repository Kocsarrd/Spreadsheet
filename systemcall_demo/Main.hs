module Main where

import Control.Monad
import Language.Haskell.Ghcid
import System.IO
import System.Process

main :: IO ()
main = do
  (ghci, _) <- startGhci "ghci" (Just ".") (\_ s -> putStrLn (s ++ "cső"))
  result <- exec ghci "3+3"
  putStrLn $ show result
  stopGhci ghci
  
