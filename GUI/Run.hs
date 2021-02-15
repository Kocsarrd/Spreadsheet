module GUI.Run where

import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import Persistence
import Spreadsheet.Spreadsheet
import Spreadsheet.Interface

runGUI :: IORef Spreadsheet -> IO ()
runGUI spreadsheet = do
  initGUI
  mainWindow <- windowNew
  vbox <- vBoxNew False 10
  set mainWindow [windowTitle := "Fazekas Sándor",
                  containerChild := vbox]

  (table, entryKeys) <- getTable spreadsheet
  editor <- getEditor spreadsheet entryKeys
  boxPackStart vbox editor PackNatural 0
  boxPackStart vbox table PackGrow 0
    
  windowMaximize mainWindow
  widgetShowAll mainWindow
  onDestroy mainWindow $ quitAndSave spreadsheet
  mainGUI
  
---------------------------------------
-- one line editor on top of the window
---------------------------------------

getEditor :: IORef Spreadsheet -> [(Entry, (Int, Int))] -> IO Entry
getEditor ssR entryKeys = do
  editor <- entryNew
  onFocusIn editor $ editorGetsFocus editor ssR
  onFocusOut editor $ editorLosesFocus editor ssR entryKeys
  return editor

editorGetsFocus :: Entry -> IORef Spreadsheet -> Event -> IO Bool
editorGetsFocus editor ssR e = do
  ss <- readIORef ssR
  case getSelected ss of
    Nothing -> entrySetText editor ""
    Just key -> entrySetText editor (getCellCode key ss)
  return False
  
editorLosesFocus :: Entry -> IORef Spreadsheet -> [(Entry, (Int, Int))] -> Event -> IO Bool
editorLosesFocus editor ssR entryKeys e = do
  ss <- readIORef ssR
  newText <- entryGetText editor
  case getSelected ss of
    Nothing -> pure ()
    Just key -> unless (newText == getCellText key ss) $
                  modifyIORef' ssR $ setCellState key newText
  forM_ entryKeys $ \(e,k) -> do
    newText <- getCellText (fromEnum k) <$> readIORef ssR
    entrySetText e newText
  return False

-----------------------------------------
-- table for representing the spreadsheet
-----------------------------------------

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
    onFocusIn entry $ cellGetsFocus entry mn spreadsheet 
    onFocusOut entry $ cellLosesFocus entry mn spreadsheet entryKeys
  return (table, entryKeys)


cellGetsFocus :: Entry -> (Int, Int) -> IORef Spreadsheet -> Event -> IO Bool
cellGetsFocus entry key ssR _ = do
  modifyIORef' ssR $ setSelected (fromEnum key)
  --debug
  ss <- readIORef ssR
  putStrLn $ "on get: " ++ show ss
  return False

cellLosesFocus :: Entry -> (Int, Int) -> IORef Spreadsheet -> [(Entry, (Int, Int))] -> Event -> IO Bool
cellLosesFocus entry key ssR entryKeys _ = do
  spreadsheet <- readIORef ssR
  entryText <- entryGetText entry
  unless (entryText == getCellText (fromEnum key) spreadsheet) $
    modifyIORef' ssR $ setCellState (fromEnum key) entryText
  
  forM_ entryKeys $ \(e,k) -> do
    newText <- getCellText (fromEnum k) <$> readIORef ssR
    entrySetText e newText
  --debug
  --putStrLn $ "on lose: " ++ show spreadsheet

  return False


-- hardcode alert
sizeX, sizeY :: Int
sizeX = 15
sizeY = 19


-- this is for debugging
quitAndSave :: IORef Spreadsheet -> IO ()
quitAndSave ssR = readIORef ssR >>= saveSheet "sheet.fsandor" >> mainQuit

-- this is for debugging
loadIfExists :: IO (IORef Spreadsheet)
loadIfExists = do
  ssE <- loadSheet "sheet.fsandor"
  case ssE of
    Left _  -> newIORef emptySpreadsheet
    Right ss -> newIORef ss 
