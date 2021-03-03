-- structured run file, will replace Run.hs
module GUI.Run2 where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Functor.Identity
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Language.Haskell.Ghcid

import Persistence
import Prelude hiding (log)
import Spreadsheet.CodeGeneration
import Spreadsheet.Types
import Spreadsheet.Interface

-- hardcode alert
sizeX, sizeY :: Int
sizeX = 15
sizeY = 19

-- types for global state
data Menubar = Menubar { saveButton :: Button
                       , loadButton :: Button
                       } deriving Eq

data Gui = Gui { mainWindow :: Window
               , log        :: TextView
               , table      :: Table
               , entryKeys  :: [(Entry,(Int,Int))]
               , editor     :: Entry
               , menu       :: Menubar
               } deriving Eq

data Env = Env { ghci  :: Ghci
               , gui   :: Gui
               , state :: IORef Spreadsheet
               } deriving Eq

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
  pure $ Gui mainWindow log table entryKeys editor (Menubar saveButton loadButton)

-- initializes global state
createEnv :: IO Env
createEnv = do
  ghci <- fst <$> startGhci "ghci" (Just ".") (\_  -> putStrLn)
  pure (Env ghci) <*> createGui <*> newIORef emptySpreadsheet

-- onDestroy may need changing, if I want to change ghci session
runApp :: ReaderT Env IO ()
runApp = do
  setupGui
  mainW <- mainWindow <$> asks gui
  ghci' <- asks ghci
  lift $ do
    windowMaximize mainW
    widgetShowAll mainW
    onDestroy mainW $ mainQuit >> stopGhci ghci'
    mainGUI

-- connects the GUI with the logic
setupGui :: ReaderT Env IO ()
setupGui = do
  setupEditor

setupEditor :: ReaderT Env IO ()
setupEditor = do
  ed <- (editor <$> asks gui)
  liftM2 onFocusIn (editor <$> asks gui) (up editorGetsFocus)
  e' <- ask
  lift $ do
    onFocusOut ed (\e -> runReaderT (editorLosesFocus e) e')
    pure undefined
  pure ()

editorGetsFocus :: Reader Env (Event -> IO Bool)
editorGetsFocus = do
  ssR <- asks state
  ed <- editor <$> asks gui
  pure $ \_ -> do
    ss <- readIORef ssR
    case getSelected ss of
      Nothing -> entrySetText ed ""
      Just key -> entrySetText ed (getCellCode key ss)
    pure False

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

updateView :: ReaderT Env IO ()
updateView = undefined
  
up :: Reader Env a -> ReaderT Env IO a
up = mapReaderT (pure . runIdentity)
