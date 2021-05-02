module App.CreateEnv (createEnv) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import Language.Haskell.Ghcid

import Eval.EvalMain
import App.Types
import Spreadsheet.Types
import Spreadsheet.Interface (emptySpreadsheet)
import Persistence

-- initializes global state
createEnv :: IO Env
createEnv = do
  evalControl <- createEvalControl
  forkIO $ evalMain evalControl
  pure (Env evalControl) <*> createGui <*> newIORef emptySpreadsheet <*> newIORef Nothing

-- initializes variables for evaluation
-- modules aren't loaded until first ghci reload
createEvalControl :: IO EvalControl
createEvalControl = EvalControl <$> (fst <$> startGhci "ghci" (Just ".") (\_  -> putStrLn) >>= newMVar)
                                <*> newEmptyMVar
                                <*> newEmptyMVar
                                <*> (mc >>= newMVar)
  where
    mc = do
      mMc <- loadModuleConfig
      case mMc of
        Nothing -> putStrLn "warning: no configuration file found" >> pure (EvalConfig [] [])
        Just mc' -> pure mc'
    
-- creates the GUI layout, without adding functionality 
createGui :: IO Gui
createGui = do
  initGUI
  mainWindow <- windowNew
  vbox <- vBoxNew False 10
  set mainWindow [windowTitle := "*new file",
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
  forM_ (take (sizeX+1) $ zip [0..] ['A'..]) $ \(n,c) -> do
    label <- labelNew $ Just ""
    labelSetMarkup label $ "<span foreground=\"white\" weight=\"bold\" >" ++ pure c ++ "</span>"
    tableAttach table label (n+1) (n+2) 0 1 [Fill] [] 0 0
  forM_ [0..sizeY] $ \n -> do 
    label <- labelNew $ Just ""
    labelSetMarkup label $ "<span foreground=\"white\" weight=\"bold\" >" ++ show n ++ "</span>"
    tableAttach table label 0 1 (n+1) (n+2) [Fill] [] 0 0
  entryKeys <- fmap concat $ forM [0..sizeX] $
    \n -> forM [0..sizeY] $ \m -> do
      entry <- entryNew
      entrySetWidthChars entry 10
      tableAttach table entry (n+1) (n+2) (m+1) (m+2) [Fill] [] 0 0
      return (entry, (m,n))
  tableWindow <- scrolledWindowNew Nothing Nothing
  set tableWindow [ scrolledWindowHscrollbarPolicy := PolicyAutomatic
                  , scrolledWindowVscrollbarPolicy := PolicyAutomatic]
  scrolledWindowAddWithViewport tableWindow table
  vpaned <- vPanedNew
  panedAdd1 vpaned tableWindow
  panedAdd2 vpaned logWindow
  editor <- entryNew
  commandLine <- entryNew
  menu <- hButtonBoxNew
  buttonBoxSetLayout menu ButtonboxStart
  newButton <- buttonNewWithMnemonic "_New"
  saveButton <- buttonNewWithMnemonic "_Save"
  loadButton <- buttonNewWithMnemonic "_Load"
  modulesButton <- buttonNewWithLabel "Modules"
  pathsButton <- buttonNewWithLabel "Search paths"
  boxPackStart menu newButton PackNatural 0
  boxPackStart menu saveButton PackNatural 0
  boxPackStart menu loadButton PackNatural 0
  boxPackStart menu modulesButton PackNatural 0
  boxPackStart menu pathsButton PackNatural 0
  boxPackStart vbox menu PackNatural 0
  boxPackStart vbox editor PackNatural 0
  --boxPackStart vbox tableWindow PackGrow 0 --
  --boxPackStart vbox logWindow PackNatural 0 --
  boxPackStart vbox vpaned PackGrow 0
  boxPackStart vbox commandLine PackNatural 0
  pure $ Gui mainWindow logWindow buffer table entryKeys editor commandLine
           (Menubar newButton saveButton loadButton modulesButton pathsButton)
