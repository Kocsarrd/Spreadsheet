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
import Eval.CommandLine
import Eval.Ghci
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

-- need update feature
-- this is a bit ugly, exec should give back EGhciError
commandLineActivated :: ReaderT Env IO ()
commandLineActivated = do
  cl <- asksGui commandLine
  command <- lift $ entryGetText cl
  lift $ entrySetText cl ""
  case parseCommand command of
    Just (ClGhci str) -> do
      result <- withReaderT evalControl $ execGhciQuery str
      case result of
        Left ETimeoutError -> logAppendText $ "query timed out: " ++ command
        Right res -> logAppendText res
        Left (EGhciError errs) -> logAppendText $ intercalate "\n" errs
    _ -> logAppendText $ "unknown command: " ++ command
    
-----------------------------
-- menubar for action buttons
-----------------------------

setupMenubar :: ReaderT Env IO ()
setupMenubar = do
  (Menubar new save load modules paths) <- asksGui menu
  env <- ask
  lift $ onClicked new $ runReaderT newAction env
  lift $ onClicked save $ runReaderT saveAction env
  lift $ onClicked load $ runReaderT loadAction env
  lift $ onClicked modules $ runReaderT (modulesAction EditModules) env
  void $ lift $ onClicked paths $ runReaderT (modulesAction EditPaths) env

-- need to add are you sure prompt 
newAction :: ReaderT Env IO ()
newAction = do
  answer <- lift $ runAreYouSureDialog
  when answer $ do
    ssR <- askState
    fileR <- askFile
    lift $ writeIORef ssR emptySpreadsheet
    lift $ writeIORef fileR $ Nothing
    setTitle "*new file"
    updateView
  
getFileChooserDialog :: FileChooserAction -> IO FileChooserDialog
getFileChooserDialog act =  fileChooserDialogNew (Just $ title ++ " sheet") Nothing act
                                   [("Cancel", ResponseCancel), (title, ResponseAccept)]
  where
    title = case act of
              FileChooserActionOpen -> "Load"
              FileChooserActionSave -> "Save"
              _                     -> "Fazekas Sándor"

-- need to add are you sure prompt
loadAction :: ReaderT Env IO ()
loadAction = do
  ssR  <- askState
  dialog <- lift $ getFileChooserDialog FileChooserActionOpen
  lift $ widgetShow dialog
  response <- lift $ dialogRun dialog
  case response of
    ResponseAccept -> do fname <- lift $ fileChooserGetFilename dialog
                         case fname of
                           Nothing -> pure ()
                           Just file -> do
                             answer <- lift $ runAreYouSureDialog
                             when answer $ do
                               lift $ loadSheet file >>= either putStrLn (writeIORef ssR)
                               fileR <- askFile
                               lift $ writeIORef fileR $ Just $ File file Saved
                               setTitle file
    _ -> pure ()
  lift $ widgetDestroy dialog
  updateView

saveAction :: ReaderT Env IO ()
saveAction = do
  mFile <- askFile >>= liftIO . readIORef
  case mFile of
    Nothing -> saveNewFile
    Just (File fp Modified) -> do
      ss <- askState >>= liftIO . readIORef
      lift $ saveSheet fp ss
    _ -> pure ()
  
saveNewFile :: ReaderT Env IO ()
saveNewFile = do
  ss <- askState >>= liftIO . readIORef
  dialog <- lift $ getFileChooserDialog FileChooserActionSave
  lift $ fileChooserSetDoOverwriteConfirmation dialog True
  lift $ widgetShow dialog
  response <- lift $ dialogRun dialog
  case response of
    ResponseAccept -> do mFname <- lift $ fileChooserGetFilename dialog
                         case mFname of
                           Nothing -> pure ()
                           Just fname -> do
                             fileR <- askFile
                             lift $ saveSheet (fname ++ ".fsandor") ss
                             lift $ writeIORef fileR $ Just $ File fname Saved
                             setTitle fname
    _ -> pure ()
  lift $ widgetDestroy dialog

runAreYouSureDialog :: IO Bool
runAreYouSureDialog = do
  dialog <- dialogNew
  windowSetTitle dialog "Are you sure? Any unsaved work will be lost!"
  dialogAddButton dialog "Yes" ResponseYes
  dialogAddButton dialog "No" ResponseNo
  ((==) ResponseYes) <$> (dialogRun dialog <* widgetDestroy dialog)

data ModuleActionType = EditPaths | EditModules
  deriving Eq

getModulesDialog :: TextBuffer -> IO Dialog
getModulesDialog buffer = do
  dialog <- dialogNew
  windowSetTitle dialog "Modules loaded to GHCi"
  text <- textViewNewWithBuffer buffer
  dialogAddActionWidget dialog text ResponseNone
  dialogAddButton dialog "Apply" ResponseApply
  pure dialog

-- does not unload any modules
modulesAction :: ModuleActionType -> ReaderT Env IO ()
modulesAction mat = do
  configR <- eConfig <$> asks evalControl 
  lift $ do
    EvalConfig ms ps <- readMVar configR
    buffer <- textBufferNew Nothing
    let (ls,ecSet) = case mat of
                       EditModules -> (ms, ecSetModules)
                       EditPaths -> (ps, ecSetPaths)
    textBufferSetText buffer $ intercalate "\n" ls 
    dialog <- getModulesDialog buffer
    widgetShowAll dialog
    response <- dialogRun dialog
    newModules <- bufferContent buffer
    c <- readMVar configR
    swapMVar configR $ ecSet (splitOn "\n" newModules) c
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
    putStrLn $ "on get: " ++ show ss
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

evalAndSet :: CellID -> ReaderT Env IO ()
evalAndSet id = do
  ssR <- askState
  g <- askGhci >>= lift . readMVar
  l <- asksGui log
  ss <- lift $ readIORef ssR
  eData <- asks evalControl
  case generateCode ss id of
    Left GenEmptyCell -> pure ()
    Left GenMissingDep ->  logAppendText "can't evaluate: missing dependencies"
    Left GenListType -> logAppendText "can't evaluate: list type error"
    Right (xs,ys) -> do
      -- 2 debug lines
      lift $ mapM_ putStrLn xs
      lift $ mapM_ (\(c,i) -> putStrLn $ show i ++ ' ' : c) ys
      unless (null ys) $ do
        withReaderT evalControl $ do
          -- needed to clear bindings
          loadModules
          mapM_ execGhciQuery xs
          results <- mapM evalOne ys
          lift $ putStrLn $ show results
          lift $ forM_ results $ \(r,i) -> modifyIORef' ssR $ cacheCell i r
  fileR <- askFile 
  lift $ modifyIORef' fileR $ fmap (setStatus Modified)
  where
    evalOne (c,i) = do
      res <- execGhciCommand c
      case res of
        Left err -> pure $ (Left err,i)
        Right _ -> (\x->(x,i)) <$> execGhciCommand ("fromJust " ++ 'v' : show i)
      
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

setTitle :: String -> ReaderT Env IO ()
setTitle str = asksGui mainWindow >>= liftIO . flip windowSetTitle str

-- generalize Reader action to ReaderT action
up :: Reader r a -> ReaderT r IO a
up = mapReaderT (pure . runIdentity)

-- convinience functions to access Env
asksGui :: Monad m => (Gui -> a) -> ReaderT Env m a
asksGui f = f <$> asks gui

askState = asks state
askGhci = eGhci <$> asks evalControl
askFile = asks file
