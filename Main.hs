module Main where

import GUI.Run
import Persistence

main :: IO ()
main = do
  spreadsheet <- loadIfExists
  runGUI spreadsheet
