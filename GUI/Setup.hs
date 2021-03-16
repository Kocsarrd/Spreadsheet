module GUI.Setup (setupGui) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.Function as DF (on) 
import Data.Functor.Identity
import Data.IORef
import Data.List (groupBy, intercalate)
import Data.List.Split (splitOn)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Language.Haskell.Ghcid
import Prelude hiding (log)

import GUI.Types
import Eval.Evaluation
import Persistence
import Spreadsheet.CodeGeneration
import Spreadsheet.Types
import Spreadsheet.Interface
import Spreadsheet.Parser

-- connects the GUI with the logic
setupGui :: ReaderT Env IO ()
setupGui = do
  setupEditor
  setupCommandLine
  setupMenubar
  setupTable

-------------------------
-- one line editor on top
-------------------------

setupEditor :: ReaderT Env IO ()
setupEditor = do
  ed <- asksGui editor
  env <- ask
  lift $ onFocusOut ed (\e -> runReaderT (editorLosesFocus e) env)
  lift $ onFocusIn ed (\e -> runReaderT (editorGetsFocus e) env)
  pure ()

editorGetsFocus :: Event -> ReaderT Env IO Bool
editorGetsFocus _ = do
  ss <- askState >>= liftIO . readIORef
  ed <- asksGui editor
  lift $ do
    case getSelected ss of
      Nothing -> entrySetText ed ""
      Just key -> entrySetText ed (getCellCode key ss)
  pure False

editorLosesFocus :: Event -> ReaderT Env IO Bool
editorLosesFocus e = do
  ssR <- askState
  ed <- asksGui editor
  ss <- lift $ readIORef ssR
  newText <- lift $ entryGetText ed 
  case getSelected ss of
    Nothing -> pure ()
    Just key -> lift (unless (newText == getCellText key ss) $
                         modifyIORef' ssR $ setCellState key newText) >>
                evalAndSet key
  updateView
  pure False

-----------------------------
-- command line on the bottom
-----------------------------

setupCommandLine :: ReaderT Env IO ()
setupCommandLine = do
  cl <- asksGui commandLine
  env <- ask
  void $ lift $ onEntryActivate cl $ runReaderT commandLineActivated env

-- dummy code
commandLineActivated :: ReaderT Env IO ()
commandLineActivated = do
  cl <- asksGui commandLine
  command <- lift $ entryGetText cl
  lift $ entrySetText cl ""
  logAppendText $ command ++ " was given"
    

-----------------------------
-- menubar for action buttons
-----------------------------

setupMenubar :: ReaderT Env IO ()
setupMenubar = do
  (Menubar save load modules) <- asksGui menu
  env <- ask
  lift $ onClicked save $ runReaderT saveAction (state env)
  lift $ onClicked load $ runReaderT loadAction env
  void $ lift $ onClicked modules $ runReaderT modulesAction env
  
getFileChooserDialog :: FileChooserAction -> IO FileChooserDialog
getFileChooserDialog act =  fileChooserDialogNew (Just $ title ++ " sheet") Nothing act
                                   [("Cancel", ResponseCancel), (title, ResponseAccept)]
  where
    title = case act of
              FileChooserActionOpen -> "Load"
              FileChooserActionSave -> "Save"
              _                     -> "Fazekas Sándor"

loadAction :: ReaderT Env IO ()
loadAction = do
  ssR  <- askState
  lift $ do
    dialog <- getFileChooserDialog FileChooserActionOpen
    widgetShow dialog
    response <- dialogRun dialog
    case response of
      ResponseAccept -> do fname <- fileChooserGetFilename dialog
                           case fname of
                             Nothing -> pure ()
                             Just file -> loadSheet file >>= either putStrLn (writeIORef ssR)      
      _ -> pure ()
    widgetDestroy dialog
  updateView

saveAction :: ReaderT (IORef Spreadsheet) IO ()
saveAction = do
  ss <- ask >>= liftIO . readIORef
  lift $ do
    dialog <- getFileChooserDialog FileChooserActionSave
    fileChooserSetDoOverwriteConfirmation dialog True
    widgetShow dialog
    response <- dialogRun dialog
    case response of
      ResponseAccept -> do fname <- fileChooserGetFilename dialog
                           case fname of
                             Nothing -> pure ()
                             Just file -> saveSheet (file ++ ".fsandor") ss
      _ -> pure ()
    widgetDestroy dialog

getModulesDialog :: TextBuffer -> IO Dialog
getModulesDialog buffer = do
  dialog <- dialogNew
  text <- textViewNewWithBuffer buffer
  dialogAddActionWidget dialog text ResponseNone
  dialogAddButton dialog "Apply" ResponseApply
  pure dialog

-- does not unload any modules
modulesAction :: ReaderT Env IO ()
modulesAction = do
  configR <- eConfig <$> asks evalControl 
  lift $ do
    EvalConfig config <- readMVar configR
    buffer <- textBufferNew Nothing
    textBufferSetText buffer $ intercalate "\n" config 
    dialog <- getModulesDialog buffer
    widgetShowAll dialog
    response <- dialogRun dialog
    newModules <- bufferContent buffer
    swapMVar configR $ EvalConfig $ splitOn "\n" newModules
    widgetDestroy dialog
  withReaderT evalControl loadModules
  where
    bufferContent :: TextBuffer -> IO String
    bufferContent buff = do 
      start <- textBufferGetStartIter buff
      end <-textBufferGetEndIter buff
      textBufferGetText buff start end False
      
----------------------------------
-- table showing the sheet content
----------------------------------

setupTable :: ReaderT Env IO ()
setupTable = do
  ek <- asksGui entryKeys
  env <- ask
  lift $ forM_ ek $ \(entry,mn) -> do
    onFocusIn entry $ (\e -> runReaderT (cellGetsFocus mn e) env)
    onFocusOut entry $ (\e -> runReaderT (cellLosesFocus entry mn e) env)

cellGetsFocus :: (Int, Int) -> Event -> ReaderT Env IO Bool
cellGetsFocus key _ = do
  ssR <- askState
  ed <- asksGui editor
  lift $ do
    modifyIORef' ssR $ setSelected (fromEnum key)
    ss <- readIORef ssR
    entrySetText ed (getCellCode (fromEnum key) ss)
    --debug:
    --putStrLn $ "on get: " ++ show ss
    pure False

cellLosesFocus :: Entry -> (Int,Int) -> Event -> ReaderT Env IO Bool
cellLosesFocus entry key _ = do
  l <- asksGui log
  ssR <- askState
  lift $ do
    ss <- readIORef ssR
    entryText <- entryGetText entry
    unless (entryText == getCellText (fromEnum key) ss) $
      modifyIORef' ssR $ setCellState (fromEnum key) entryText
  evalAndSet (fromEnum key)
  updateView
  pure False

-----------------------------
-- actions called by handlers
-----------------------------

-- most of functionality should be moved to Eval
evalAndSet :: CellID -> ReaderT Env IO ()
evalAndSet id = do
  ssR <- askState
  g <- askGhci >>= lift . readMVar
  l <- asksGui log
  ss <- lift $ readIORef ssR
  eData <- asks evalControl
  case generateCode ss id of
    Left GenMissingDep ->  logAppendText "can't evaluate: missing dependencies"
    Left GenListType -> logAppendText "can't evaluate: list type error"
    Right (code,ids) -> unless (code == "()") $ do
      lift $ putStrLn code -- !! debug line
      result <- withReaderT evalControl $ execGhciCommand code
      case result of
        Left _ -> do
          lift $ modifyIORef' ssR (cacheCell id $ Left ETimeoutError)
          logAppendText $ show result
        Right [res] -> lift $ forM_ (zip ids (getResult res))
                                (\(i,c) -> modifyIORef' ssR $ cacheCell i $ Right c)
        _    -> do
          lift $ modifyIORef' ssR (cacheCell id $ Left EGhciError)
          logAppendText $ show result 

-- this should support keeping the log shorter than a max number of lines
logAppendText :: String -> ReaderT Env IO ()
logAppendText str = do
  l <- asksGui log
  sw <- logWindow <$> asks gui
  lift $ do
    textBufferInsertAtCursor l (str ++ "\n")
    mVbar <- scrolledWindowGetVScrollbar sw
    case mVbar of
      -- this case should never be reached
      Nothing -> putStrLn "what is a vscrollbar lol"
      Just vbar -> do
        adj <- rangeGetAdjustment vbar
        u <- adjustmentGetUpper adj
        p <- adjustmentGetPageSize adj
        adjustmentSetValue adj (u-p)
      
updateView :: ReaderT Env IO ()
updateView = do
  ek <- asksGui entryKeys
  ss <- askState >>= liftIO . readIORef
  l <- asksGui log
  lift $ forM_ ek $ \(e,k) ->
    entrySetText e $ getCellText (fromEnum k) ss
  logAppendText $ getLogMessage ss

-- generalize Reader action to ReaderT action
up :: Reader r a -> ReaderT r IO a
up = mapReaderT (pure . runIdentity)

-- convinience functions to access Env
asksGui :: Monad m => (Gui -> a) -> ReaderT Env m a
asksGui f = f <$> asks gui

askState = asks state
askGhci = eGhci <$> asks evalControl
