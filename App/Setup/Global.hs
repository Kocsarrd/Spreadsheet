module App.Setup.Global
  ( evalAndSet
  , logAppendText
  , updateView
  , setTitle
  , asksGui, askState, askGhci, askFile
  , runAreYouSureDialog
  , module App.Types
  , module Spreadsheet.Interface
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Data.Functor.Identity
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Prelude hiding (log)

import App.Types
import Eval.CodeGeneration
import Eval.Ghci
import Spreadsheet.Interface

-----------------------------
-- actions called by handlers
-----------------------------

evalAndSet :: CellID -> App ()
evalAndSet id = do
  ssR <- askState
  g <- askGhci >>= lift . readMVar
  l <- asksGui log
  ss <- lift $ readIORef ssR
  eData <- asks evalControl
  case generateCode ss id of
    Left (GenEmptyCell,_) -> pure ()
    Left (GenMissingDep, cs) -> do
      logAppendText "can't evaluate: missing dependencies"
      lift $ forM_ cs $ modifyIORef ssR . flip cacheCell (Left EMissingDepError) 
    Left (GenListType,_) -> logAppendText "can't evaluate: list type error"
    Right (xs,ys) -> do
      -- 2 debug lines
      --lift $ mapM_ putStrLn xs
      --lift $ mapM_ (\(c,i) -> putStrLn $ show i ++ ' ' : c) ys
      unless (null ys) $ do
          -- needed to clear bindings
        withReaderT evalControl $ loadModules >> mapM_ execGhciQuery xs
        results <- mapM evalOne ys
        --lift $ putStrLn $ show results
        lift $ forM_ results $ \(r,i) -> modifyIORef' ssR $ cacheCell i r
  fileR <- askFile 
  lift $ modifyIORef' fileR $ fmap (setStatus Modified)
  where
    evalOne (c,i) = do
      res <- withReaderT evalControl $ execGhciCommand c
      -- lift $ putStrLn $ show res
      case res of
        Left err -> showErr err >> (pure  (Left err,i))
        Right _ -> fmap (\x->(x,i)) $ withReaderT evalControl $ execGhciCommand ("fromJust " ++ 'v' : show i)
    showErr (EGhciError xs) = mapM_  logAppendText xs
    showErr ETimeoutError = logAppendText "evaluation timed out"
    showErr x = logAppendText $ show x
    
-- this should support keeping the log shorter than a max number of lines
logAppendText :: String -> App ()
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
      
updateView :: App ()
updateView = do
  ek <- asksGui entryKeys
  ss <- askState >>= liftIO . readIORef
  l <- asksGui log
  lift $ forM_ ek $ \(e,(k1,k2)) ->
    entrySetText e $ getCellText (fromEnum (k2,k1)) ss
  --logAppendText $ getLogMessage ss
  setTitle

runAreYouSureDialog :: IO Bool
runAreYouSureDialog = do
  dialog <- dialogNew
  windowSetTitle dialog "Are you sure? Any unsaved work will be lost!"
  dialogAddButton dialog "Yes" ResponseYes
  dialogAddButton dialog "No" ResponseNo
  ((==) ResponseYes) <$> (dialogRun dialog <* widgetDestroy dialog)

setTitle :: App ()
setTitle = do
  mw <- asksGui mainWindow
  file <- askFile >>= liftIO . readIORef
  let str = case file of
               Just (File fname Saved) -> fname ++ ".fsandor"
               Just (File fname Modified) -> '*' : fname ++ ".fsandor"
               Nothing -> "*new file"
  lift $ windowSetTitle mw str

-- generalize Reader action to ReaderT action
up :: Reader r a -> ReaderT r IO a
up = mapReaderT (pure . runIdentity)

-- convinience functions to access Env
asksGui :: (Gui -> a) -> App a
asksGui f = f <$> asks gui

askState = asks state
askGhci = eGhci <$> asks evalControl
askFile = asks file
