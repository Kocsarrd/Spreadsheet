module GUI.Run where

import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import Persistence
import Spreadsheet.Spreadsheet
import Spreadsheet.Interface

runGUI :: IORef Spreadsheet -> IO ()
runGUI ssR = do
  initGUI
  mainWindow <- windowNew
  vbox <- vBoxNew False 10
  set mainWindow [windowTitle := "Fazekas Sándor",
                  containerChild := vbox]

  menu <- getMenubar ssR
  (table, entryKeys) <- getTable ssR
  editor <- getEditor ssR entryKeys
  boxPackStart vbox menu PackNatural 0
  boxPackStart vbox editor PackNatural 0
  boxPackStart vbox table PackGrow 0
    
  windowMaximize mainWindow
  widgetShowAll mainWindow
  onDestroy mainWindow mainQuit
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

--------------------------------------
-- file chooser for loading and saving
--------------------------------------

getFileChooserDialog :: FileChooserAction -> IO FileChooserDialog
getFileChooserDialog act =  fileChooserDialogNew (Just $ title ++ "sheet") Nothing act
                                   [("Cancel", ResponseCancel), (title, ResponseAccept)]                                
  where
    title = case act of
              FileChooserActionOpen -> "Load"
              FileChooserActionSave -> "Save"
              _                     -> "Fazekas Sándor"

----------------------------
-- menubar for basic actions
----------------------------

getMenubar :: IORef Spreadsheet -> IO HButtonBox
getMenubar ssR = do
  menu <- hButtonBoxNew
  buttonBoxSetLayout menu ButtonboxStart
  saveButton <- buttonNewWithMnemonic "_Save"
  onClicked saveButton $ saveAction ssR
  loadButton <- buttonNewWithMnemonic "_Load"
  onClicked loadButton $ loadAction ssR
  boxPackStart menu saveButton PackNatural 0
  boxPackStart menu loadButton PackNatural 0
  return menu
  
  
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

------------------------------------------
-- persistence actions, this will be moved
------------------------------------------

loadAction :: IORef Spreadsheet -> IO ()
loadAction ssR = do
  dialog <- getFileChooserDialog FileChooserActionOpen
  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do fname <- fileChooserGetFilename dialog
                         case fname of
                           Nothing -> pure ()
                           Just file -> do ssE <- loadSheet file
                                           either putStrLn (writeIORef ssR)
    _ -> pure ()
  widgetDestroy dialog

saveAction :: IORef Spreadsheet -> IO ()
saveAction ssR = do
  dialog <- getFileChooserDialog FileChooserActionSave
  fileChooserSetDoOverwriteConfirmation dialog True
  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do fname <- fileChooserGetFilename dialog
                         case fname of
                           Nothing -> pure ()
                           Just file -> readIORef ssR >>= saveSheet (file ++ ".fsandor")
    _ -> pure ()
  widgetDestroy dialog
  

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
