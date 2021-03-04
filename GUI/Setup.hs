module GUI.Setup (setupGui) where

import Control.Monad
import Control.Monad.Reader
import qualified Data.Function as DF (on) 
import Data.Functor.Identity
import Data.IORef
import Data.List (groupBy)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Language.Haskell.Ghcid
import Prelude hiding (log)

import GUI.Types
import Persistence
import Spreadsheet.CodeGeneration
import Spreadsheet.Types
import Spreadsheet.Interface
import Spreadsheet.Parser

-- connects the GUI with the logic
setupGui :: ReaderT Env IO ()
setupGui = do
  setupEditor
  setupMenubar
  setupTable

-------------------------
-- one line editor on top
-------------------------

setupEditor :: ReaderT Env IO ()
setupEditor = do
  ed <- (editor <$> asks gui)
  env <- ask
  lift $ onFocusOut ed (\e -> runReaderT (editorLosesFocus e) env)
  lift $ onFocusIn ed (\e -> runReaderT (editorGetsFocus e) env)
  pure ()

editorGetsFocus :: Event -> ReaderT Env IO Bool
editorGetsFocus _ = do
  ss <- asks state >>= liftIO . readIORef
  ed <- editor <$> asks gui
  lift $ do
    case getSelected ss of
      Nothing -> entrySetText ed ""
      Just key -> entrySetText ed (getCellCode key ss)
  pure False

-- known bug: evaluation needs to be called here
editorLosesFocus :: Event -> ReaderT Env IO Bool
editorLosesFocus e = do
  ssR <- asks state
  ed <- editor <$> asks gui
  lift $ do
    ss <- readIORef ssR
    newText <- entryGetText ed
    case getSelected ss of
      Nothing -> pure ()
      Just key -> unless (newText == getCellText key ss) $
                  modifyIORef' ssR $ setCellState key newText
  updateView
  pure False

-----------------------------
-- menubar for action buttons
-----------------------------

setupMenubar :: ReaderT Env IO ()
setupMenubar = do
  (Menubar save load) <- menu <$> asks gui
  env <- ask
  lift $ onClicked save $ runReaderT saveAction (state env)
  void $ lift $ onClicked load $ runReaderT loadAction env

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
  ssR  <- asks state
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

----------------------------------
-- table showing the sheet content
----------------------------------

setupTable :: ReaderT Env IO ()
setupTable = do
  ek <- entryKeys <$> asks gui
  env <- ask
  lift $ forM_ ek $ \(entry,mn) -> do
    onFocusIn entry $ (\e -> runReaderT (cellGetsFocus mn e) (state env))
    onFocusOut entry $ (\e -> runReaderT (cellLosesFocus entry mn e) env)

cellGetsFocus :: (Int, Int) -> Event -> ReaderT (IORef Spreadsheet) IO Bool
cellGetsFocus key _ = do
  ssR <- ask
  lift $ do
    modifyIORef' ssR $ setSelected (fromEnum key)
    --debug:
    --ss <- readIORef ssR
    --putStrLn $ "on get: " ++ show ss
    pure False

cellLosesFocus :: Entry -> (Int,Int) -> Event -> ReaderT Env IO Bool
cellLosesFocus entry key _ = do
  l <- log <$> asks gui
  ssR <- asks state
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

-- this is incomplete (timeout)
-- parser call should be moved to cacheCell in Interface
evalAndSet :: CellID -> ReaderT Env IO ()
evalAndSet id = do
  ssR <- asks state
  g <- asks ghci
  l <- log <$> asks gui
  lift $ do
    ss <- readIORef ssR
    case generateCode ss id of
      Left GenMissingDep -> textBufferSetText l "can't evaluate: missing dependencies"
      Left GenListType -> textBufferSetText l "can't evaluate: list type error"
      Right (code,ids) -> unless (code == "()") $ do
        putStrLn code -- !!
        result <- exec g code
        case result of
          [res] -> forM_ (zip ids (getResult res)) (\(i,c) -> modifyIORef' ssR $ cacheCell i $ Right c)
          _    -> textBufferSetText l (show result) >>
                  modifyIORef' ssR (cacheCell id $ Left EGhciError)
        textBufferSetText l (show result) 
  
updateView :: ReaderT Env IO ()
updateView = do
  ek <- entryKeys <$> asks gui
  ss <- asks state >>= liftIO . readIORef
  l <- log <$> asks gui
  lift $ do
    forM_ ek $ \(e,k) ->
      entrySetText e $ getCellText (fromEnum k) ss
    --textBufferSetText l $ getLogMessage ss

--
up :: Reader Env a -> ReaderT Env IO a
up = mapReaderT (pure . runIdentity)
