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
               , log        :: TextBuffer
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
  pure $ Gui mainWindow buffer table entryKeys editor (Menubar saveButton loadButton)

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
  setupMenubar
  setupTable

setupEditor :: ReaderT Env IO ()
setupEditor = do
  ed <- (editor <$> asks gui)
  env <- ask
  lift $ onFocusOut ed (\e -> runReaderT (editorLosesFocus e) env)
  lift $ onFocusIn ed (\e -> runReaderT (editorGetsFocus e) env)
  pure ()

editorGetsFocus :: Event -> ReaderT Env IO Bool
editorGetsFocus _ = do
  ssR <- asks state
  ed <- editor <$> asks gui
  lift $ do
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

setupMenubar :: ReaderT Env IO ()
setupMenubar = do
  (Menubar save load) <- menu <$> asks gui
  e' <- ask
  lift $ onClicked save $ runReaderT saveAction (state e')
  void $ lift $ onClicked load $ runReaderT loadAction e'

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
  ssR <- ask
  lift $ do
    dialog <- getFileChooserDialog FileChooserActionSave
    fileChooserSetDoOverwriteConfirmation dialog True
    widgetShow dialog
    response <- dialogRun dialog
    case response of
      ResponseAccept -> do fname <- fileChooserGetFilename dialog
                           case fname of
                             Nothing -> pure ()
                             Just file -> readIORef ssR >>= saveSheet (file ++ ".fsandor")
      _ -> pure ()
    widgetDestroy dialog

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
    ss <- readIORef ssR
    putStrLn $ "on get: " ++ show ss
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
      Right (code,ids) -> (show <$> exec g code) >>=
                          (\s -> textBufferSetText l (s ++ show id))
  

updateView :: ReaderT Env IO ()
updateView = do
  ek <- entryKeys <$> asks gui
  ssR <- asks state
  l <- log <$> asks gui
  lift $ do
    ss <- readIORef ssR
    forM_ ek $ \(e,k) ->
      entrySetText e $ getCellText (fromEnum k) ss
    textBufferSetText l $ getLogMessage ss

up :: Reader Env a -> ReaderT Env IO a
up = mapReaderT (pure . runIdentity)
