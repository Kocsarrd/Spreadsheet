module App.Setup.Table (setupTable) where

import Control.Monad.Reader
import Data.IORef
import Data.List (find)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

import App.Setup.Global
import Spreadsheet.Interface

----------------------------------
-- table showing the sheet content
----------------------------------

setupTable :: App ()
setupTable = do
  ek <- asksGui entryKeys
  env <- ask
  lift $ forM_ ek $ \(entry,mn) -> do
    onFocusIn entry $ (\e -> runReaderT (cellGetsFocus mn e) env)
    onFocusOut entry $ (\e -> runReaderT (cellLosesFocus entry mn) env)
    void $ onEntryActivate entry $ void $ runReaderT (cellLosesFocus entry mn) env
  cbk <- asksGui colButtonKeys
  lift $ forM_ cbk $ \(button, c) ->
    onClicked button $ runReaderT (colButtonClicked c) env
    

cellGetsFocus :: (Int, Int) -> Event -> App Bool
cellGetsFocus (k1,k2) _ = do
  ssR <- askState
  ed <- asksGui editor
  lift $ do
    modifyIORef' ssR $ setSelected (fromEnum (k2,k1))
    ss <- readIORef ssR
    entrySetText ed (getCellCode (fromEnum (k2,k1)) ss)
    --debug:
    --putStrLn $ "on get: " ++ show ss
    pure False

cellLosesFocus :: Entry -> (Int,Int) -> App Bool
cellLosesFocus entry (k1,k2)  = do
  ssR <- askState
  lift $ do
    ss <- readIORef ssR
    entryText <- entryGetText entry
    unless (entryText == getCellText (fromEnum (k2,k1)) ss) $
      modifyIORef' ssR $ setCellState (fromEnum (k2,k1)) entryText
  evalAndSet (fromEnum (k2,k1))
  updateView
  pure False

-- if-then-else's else case is not needed
-- it's only a safety measure
colButtonClicked :: Char -> App ()
colButtonClicked col = do
  entries <- map fst . filter ((==fromEnum col).(+65).snd.snd) <$> asksGui entryKeys
  lift $ do
    curSize <- if not $ null entries then entryGetWidthChars $ head entries else pure 10
    mNewSize <- runColSizeDialog curSize
    case mNewSize of
      Just newSize -> forM_ entries $ \e -> entrySetWidthChars e newSize
      Nothing -> pure ()
      
runColSizeDialog :: Int -> IO (Maybe Int)
runColSizeDialog n = do
  dialog <- dialogNew
  windowSetTitle dialog "Set column width"
  spinButton <- spinButtonNewWithRange 0 100 1
  set spinButton [ spinButtonNumeric := True , spinButtonValue := fromIntegral n]
  dialogAddActionWidget dialog spinButton ResponseApply
  dialogAddButton dialog "Apply" ResponseApply
  widgetShowAll dialog
  result <- dialogRun dialog
  case result of
    ResponseApply -> do
      widgetDestroy dialog
      Just <$> spinButtonGetValueAsInt spinButton
    _ -> pure Nothing
