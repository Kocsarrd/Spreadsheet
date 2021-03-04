import Control.Monad.Reader

data Env = Env { first :: Int,
                 second :: Int }
  deriving(Eq,Show)

main :: IO ()
main = runReaderT stuff $ Env 2 3

stuff :: ReaderT Env IO ()
stuff = do
  a <- lift getLine
  str <- stuff2 a
  lift $ putStrLn str

stuff2 a = do
  as <- ask
  pure $ show as ++ a 
