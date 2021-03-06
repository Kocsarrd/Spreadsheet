module GUI.CreateEnv (createEnv) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import Language.Haskell.Ghcid

import Eval.EvalMain
import GUI.Types
import Spreadsheet.Types
import Spreadsheet.Interface (emptySpreadsheet)

-- initializes global state
createEnv :: IO Env
createEnv = do
  evalData <- createEvalData
  --forkIO evalThread evalData
  pure (Env evalData) <*> createGui <*> newIORef emptySpreadsheet

-- initializes variables for evaluation
createEvalData :: IO EvalData
createEvalData = EvalData <$> (fst <$> startGhci "ghci" (Just ".") (\_  -> putStrLn) >>= newMVar)
                          <*> newEmptyMVar
                          <*> newEmptyMVar

-- creates the GUI layout, without adding functionality 
createGui :: IO Gui
createGui = do
  initGUI
  mainWindow <- windowNew
  vbox <- vBoxNew False 10
  set mainWindow [windowTitle := "Fazekas Sándor",
                  containerChild := vbox]
  logWindow <- scrolledWindowNew Nothing Nothing
  log <- textViewNew
  set log [ textViewEditable := False
          , textViewLeftMargin := 10
          , textViewRightMargin := 10
          ]
  set logWindow [ containerChild := log
                , scrolledWindowHscrollbarPolicy := PolicyNever]
  buffer <- textViewGetBuffer log
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
  editor <- entryNew
  menu <- hButtonBoxNew
  buttonBoxSetLayout menu ButtonboxStart
  saveButton <- buttonNewWithMnemonic "_Save"
  loadButton <- buttonNewWithMnemonic "_Load"
  boxPackStart menu saveButton PackNatural 0
  boxPackStart menu loadButton PackNatural 0
  boxPackStart vbox menu PackNatural 0
  boxPackStart vbox editor PackNatural 0
  boxPackStart vbox table PackGrow 0
  boxPackStart vbox logWindow PackGrow 0
  pure $ Gui mainWindow logWindow buffer table entryKeys editor (Menubar saveButton loadButton)
