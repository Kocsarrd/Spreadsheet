module App.Setup.Menubar (setupMenubar) where

import Control.Concurrent
import Control.Monad.Reader
import Data.IORef
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import App.Setup.Global
import Eval.Ghci
import Persistence
import Spreadsheet.Interface

-----------------------------
-- menubar for action buttons
-----------------------------

setupMenubar :: App ()
setupMenubar = do
  (Menubar new save load modules paths) <- asksGui menu
  env <- ask
  lift $ onClicked new $ runReaderT newAction env
  lift $ onClicked save $ runReaderT saveAction env
  lift $ onClicked load $ runReaderT loadAction env
  lift $ onClicked modules $ runReaderT (modulesAction EditModules) env
  void $ lift $ onClicked paths $ runReaderT (modulesAction EditPaths) env

newAction :: App ()
newAction = do
  answer <- lift $ runAreYouSureDialog
  when answer $ do
    ssR <- askState
    fileR <- askFile
    lift $ writeIORef ssR emptySpreadsheet
    lift $ writeIORef fileR $ Nothing
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
loadAction :: App ()
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
    _ -> pure ()
  lift $ widgetDestroy dialog
  updateView

saveAction :: App ()
saveAction = do
  mFileR <- askFile
  mFile <- lift $ readIORef mFileR
  case mFile of
    Nothing -> saveNewFile
    Just (File fp Modified) -> do
      ss <- askState >>= liftIO . readIORef
      lift $ saveSheet fp ss
      lift $ putStrLn "lolcsi"
      lift $ writeIORef mFileR $ Just $ File fp Saved
      setTitle
    _ -> pure ()
  
saveNewFile :: App ()
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
                             setTitle
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
modulesAction :: ModuleActionType -> App ()
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
