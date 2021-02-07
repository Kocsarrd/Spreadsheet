module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad
import Data.IORef
import Prelude hiding (lookup)

import Spreadsheet


main :: IO ()
main = do
  initGUI
  mainWindow <- windowNew
  vbox <- vBoxNew False 10
  set mainWindow [windowTitle := "GUI demo",
                  containerChild := vbox]

  spreadsheet <- newIORef $ emptySpreadSheet
  (table, entryKeys) <- getTable spreadsheet
  boxPackStart vbox table PackGrow 0
    
  windowMaximize mainWindow
  widgetShowAll mainWindow
  onDestroy mainWindow mainQuit
  mainGUI

getTable :: IORef Spreadsheet -> IO (Table, [(Entry, (Int, Int))])
getTable spreadsheet = do
  table <- tableNew (sizeY+2) (sizeX+1) True
  forM_ [0..sizeX] $ \n -> do
    label <- labelNew $ Just ""
    labelSetMarkup label $ "<span foreground=\"white\" weight=\"bold\" >" ++ show n ++ "</span>"
    tableAttach table label (n+1) (n+2) 0 1 [Fill] [] 0 0
  forM_ (take 20 $ zip [0..] ['A'..]) $ \(n,c) -> do
    label <- labelNew $ Just ""
    labelSetMarkup label $ "<span foreground=\"white\" weight=\"bold\" >" ++ pure c ++ "</span>"
    tableAttach table label 0 1 (n+1) (n+2) [Fill] [] 0 0
  
  entryKeys <- fmap concat $ forM [0..sizeX] $
    \n -> forM [0..sizeY] $ \m -> do
      entry <- entryNew
      entrySetWidthChars entry 10
      tableAttach table entry (n+1) (n+2) (m+1) (m+2) [Fill] [] 0 0
      return (entry, (m,n))
  forM_ entryKeys $ \(entry, mn) -> do
    afterFocusOut entry $ entryLosesFocus entry mn spreadsheet entryKeys   
  return (table, entryKeys)

entryLosesFocus :: Entry -> (Int, Int) -> IORef Spreadsheet -> [(Entry, (Int, Int))] -> Event -> IO Bool
entryLosesFocus entry key spreadsheetR entryKeys e = do
  spreadsheet <- readIORef spreadsheetR
  entryText <- entryGetText entry
  unless (entryText == getCellText (fromEnum key) spreadsheet) $
    modifyIORef' spreadsheetR $ setCellState (fromEnum key) entryText
  
  forM_ entryKeys $ \(e,k) -> do
    newText <- getCellText (fromEnum k) <$> readIORef spreadsheetR
    entrySetText e newText
  --debug
  putStrLn $ show spreadsheet
  return False

-- hardcode alert
sizeX, sizeY :: Int
sizeX = 15
sizeY = 19
