module Persistence where

import qualified Data.ByteString as BS (writeFile, readFile)
import Data.Serialize
import System.Directory

import GUI.Types (EvalConfig(..))

saveSheet :: Serialize a => String -> a -> IO ()
saveSheet fname ss = BS.writeFile fname ssSerialized where
  ssSerialized = encode ss

loadSheet :: Serialize a => String -> IO (Either String a)
loadSheet fname = do
  exists <- doesFileExist fname
  if exists
    then decode <$> BS.readFile fname
    else return $ Left $ "file: " ++ fname ++ " does not exist"

moduleConfigFile = "modules.sanyi"

saveModuleConfig :: EvalConfig -> IO ()
saveModuleConfig = saveSheet moduleConfigFile

loadModuleConfig :: IO EvalConfig
loadModuleConfig = do
  config <- loadSheet moduleConfigFile
  case config of
    Left _ -> pure $ EvalConfig [] []
    Right c -> pure c


