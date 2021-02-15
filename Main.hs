module Main where

import Data.IORef
import GUI.Run
import Persistence
import Spreadsheet.Interface

main :: IO ()
main = do
  spreadsheet <- newIORef emptySpreadsheet
  runGUI spreadsheet
