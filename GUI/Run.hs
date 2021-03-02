-- this file is a complete mess 
module GUI.Run where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Language.Haskell.Ghcid
import System.Timeout

import Persistence
import Spreadsheet.CodeGeneration
import Spreadsheet.Types
import Spreadsheet.Interface

type ViewUpdateData = (TextBuffer, [(Entry, (Int, Int))])

runGUI :: IORef Spreadsheet -> IO ()
runGUI ssR = do
  initGUI
  mainWindow <- windowNew
  vbox <- vBoxNew False 10
  set mainWindow [windowTitle := "Fazekas Sándor",
                  containerChild := vbox]
  (logWindow, log) <- getLog
  buffer <- textViewGetBuffer log

  ghci <- initGhci buffer
  mvar <- newEmptyMVar
  forkIO $ timeoutControlThread ghci mvar
  
  (table, entryKeys) <- getTable ssR buffer ghci mvar
  let vad = (buffer, entryKeys)
  menu <- getMenubar ssR vad
  editor <- getEditor ssR vad ghci mvar
  boxPackStart vbox menu PackNatural 0
  boxPackStart vbox editor PackNatural 0
  boxPackStart vbox table PackGrow 0
  boxPackStart vbox logWindow PackGrow 0
    
  windowMaximize mainWindow
 
  widgetShowAll mainWindow
  onDestroy mainWindow (mainQuit >> stopGhci ghci)
  mainGUI

----------------------------------------
-- ghci related functions, will be moved
----------------------------------------

evaluate :: IORef Spreadsheet -> CellID -> Ghci -> MVar () -> TextBuffer -> IO ()
evaluate ssR id ghci mvar log = do
  ss <- readIORef ssR
  case generateCode ss id of
    Left GenMissingDep -> textBufferSetText log "can't evaluate: missing dependencies"
    Left GenListType -> textBufferSetText log "can't evaluate: list type error"
    Right (code,ids) -> do
      unless (code == "()") $ do
        putStrLn $ code ++":)"
        result <- (timeout 2000000 $ exec ghci code)
        putStrLn $ show result
        case result of
          Nothing -> putMVar mvar () >> (modifyIORef' ssR $ \ss' -> foldr (\i s -> cacheCell i (Left ETimeoutError) s) ss' ids)
          Just yeah -> modifyIORef' ssR $ \ss' -> foldr (\i s -> cacheCell i (Right "cső") s) ss' ids
      
initGhci :: TextBuffer -> IO Ghci
initGhci log = fst <$> startGhci "ghci" (Just ".") (\_ _ -> textBufferSetText log "Started GHCi session")

timeoutControlThread :: Ghci -> MVar () -> IO ()
timeoutControlThread ghci mvar = forever $ takeMVar mvar >> interrupt ghci >> putStrLn "interrupted ghci"

---------------------------------------
-- one line editor on top of the window
---------------------------------------

getEditor :: IORef Spreadsheet -> ViewUpdateData -> Ghci -> MVar () -> IO Entry
getEditor ssR vad ghci mvar = do
  editor <- entryNew
  onFocusIn editor $ editorGetsFocus editor ssR
  onFocusOut editor $ editorLosesFocus editor ssR vad ghci mvar
  return editor

editorGetsFocus :: Entry -> IORef Spreadsheet -> Event -> IO Bool
editorGetsFocus editor ssR e = do
  ss <- readIORef ssR
  case getSelected ss of
    Nothing -> entrySetText editor ""
    Just key -> entrySetText editor (getCellCode key ss)
  return False
  
editorLosesFocus :: Entry -> IORef Spreadsheet -> ViewUpdateData -> Ghci -> MVar () -> Event -> IO Bool
editorLosesFocus editor ssR vad@(log,_) ghci mvar _ = do
  ss <- readIORef ssR
  newText <- entryGetText editor
  case getSelected ss of
    Nothing -> pure ()
    Just key -> unless (newText == getCellText key ss) $
                  modifyIORef' ssR $ setCellState key newText
  updateView ssR vad
  --test line
  case getSelected ss of
    Nothing -> pure ()
    Just key -> evaluate ssR key ghci mvar log 
  return False

------------------------------------
-- multiline text widget for logging
------------------------------------

getLog :: IO (ScrolledWindow, TextView)
getLog = do
  log <- textViewNew
  set log [ textViewEditable := False
          , textViewLeftMargin := 10
          , textViewRightMargin := 10
          ]
  logWindow <- scrolledWindowNew Nothing Nothing
  set logWindow [ containerChild := log
                , scrolledWindowHscrollbarPolicy := PolicyNever]
  return (logWindow, log)


--------------------------------------
-- file chooser for loading and saving
--------------------------------------

getFileChooserDialog :: FileChooserAction -> IO FileChooserDialog
getFileChooserDialog act =  fileChooserDialogNew (Just $ title ++ " sheet") Nothing act
                                   [("Cancel", ResponseCancel), (title, ResponseAccept)]                                
  where
    title = case act of
              FileChooserActionOpen -> "Load"
              FileChooserActionSave -> "Save"
              _                     -> "Fazekas Sándor"

----------------------------
-- menubar for basic actions
----------------------------

getMenubar :: IORef Spreadsheet -> ViewUpdateData -> IO HButtonBox
getMenubar ssR vad = do
  menu <- hButtonBoxNew
  buttonBoxSetLayout menu ButtonboxStart
  saveButton <- buttonNewWithMnemonic "_Save"
  onClicked saveButton $ saveAction ssR
  loadButton <- buttonNewWithMnemonic "_Load"
  onClicked loadButton $ loadAction ssR vad
  boxPackStart menu saveButton PackNatural 0
  boxPackStart menu loadButton PackNatural 0
  return menu
  
  
-----------------------------------------
-- table for representing the spreadsheet
-----------------------------------------

getTable :: IORef Spreadsheet -> TextBuffer -> Ghci -> MVar () -> IO (Table, [(Entry, (Int, Int))])
getTable spreadsheet log ghci mvar = do
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
    onFocusOut entry $ cellLosesFocus entry mn spreadsheet (log, entryKeys) ghci mvar
  return (table, entryKeys)


cellGetsFocus :: Entry -> (Int, Int) -> IORef Spreadsheet -> Event -> IO Bool
cellGetsFocus entry key ssR _ = do
  modifyIORef' ssR $ setSelected (fromEnum key)
  --debug
  ss <- readIORef ssR
  putStrLn $ "on get: " ++ show ss
  return False

cellLosesFocus :: Entry -> (Int, Int) -> IORef Spreadsheet -> ViewUpdateData -> Ghci -> MVar () -> Event -> IO Bool
cellLosesFocus entry key ssR vad@(log,_) ghci mvar  _ = do
  spreadsheet <- readIORef ssR
  entryText <- entryGetText entry
  unless (entryText == getCellText (fromEnum key) spreadsheet) $
    modifyIORef' ssR $ setCellState (fromEnum key) entryText
  updateView ssR vad
  --test line
  evaluate ssR (fromEnum key) ghci mvar log
  return False


-- hardcode alert
sizeX, sizeY :: Int
sizeX = 15
sizeY = 19

-----------------------------------------
-- update view based on spreadsheet state
-----------------------------------------

updateView :: IORef Spreadsheet -> ViewUpdateData -> IO ()
updateView ssR (log, entryKeys) = do
  ss <- readIORef ssR
  forM_ entryKeys $ \(e,k) ->
    entrySetText e $ getCellText (fromEnum k) ss
  textBufferSetText log $ getLogMessage ss

------------------------------------------
-- persistence actions, this will be moved
------------------------------------------

loadAction :: IORef Spreadsheet -> ViewUpdateData -> IO ()
loadAction ssR vad = do
  dialog <- getFileChooserDialog FileChooserActionOpen
  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do fname <- fileChooserGetFilename dialog
                         case fname of
                           Nothing -> pure ()
                           Just file -> loadSheet file >>= either putStrLn (writeIORef ssR)
                                        >> updateView ssR vad
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
