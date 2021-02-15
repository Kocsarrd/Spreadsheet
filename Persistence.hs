module Persistence where

import qualified Data.ByteString as BS (writeFile, readFile)
import Data.Serialize
import System.Directory

saveSheet :: Serialize a => String -> a -> IO ()
saveSheet fname ss = BS.writeFile fname ssSerialized where
  ssSerialized = encode ss

loadSheet :: Serialize a => String -> IO (Either String a)
loadSheet fname = do
  exists <- doesFileExist fname
  if exists
    then decode <$> BS.readFile fname
    else return $ Left $ "file: " ++ fname ++ " does not exist"
