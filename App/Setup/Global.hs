module App.Setup.Global where

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
import Spreadsheet.Types
import Spreadsheet.Interface

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
  lift $ forM_ ek $ \(e,(k1,k2)) ->
    entrySetText e $ getCellText (fromEnum (k2,k1)) ss
  logAppendText $ getLogMessage ss
  setTitle

setTitle :: ReaderT Env IO ()
setTitle = do
  mw <- asksGui mainWindow
  file <- askFile >>= liftIO . readIORef
  let str = case file of
               Just (File fname Saved) -> fname
               Just (File fname Modified) -> '*' : fname
               Nothing -> "*new file"
  lift $ windowSetTitle mw str

-- generalize Reader action to ReaderT action
up :: Reader r a -> ReaderT r IO a
up = mapReaderT (pure . runIdentity)

-- convinience functions to access Env
asksGui :: Monad m => (Gui -> a) -> ReaderT Env m a
asksGui f = f <$> asks gui

askState = asks state
askGhci = eGhci <$> asks evalControl
askFile = asks file
