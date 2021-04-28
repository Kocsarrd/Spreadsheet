module Test.Spreadsheet.Parser where

import Control.Monad
import Spreadsheet.Parser
import Spreadsheet.Types

runSpreadsheetParserTests :: IO ()
runSpreadsheetParserTests = do
  putStrLn "-----------------------------------"
  putStrLn "Spreadsheet.Parser -- rep function"
  putStrLn ""
  putStrLn "Testing empty string..."
  testRunner testsEmpty
  putStrLn ""
  putStrLn "Testing number literals..."
  testRunner testsNum
  putStrLn ""
  putStrLn "Testing formulas..."
  testRunner testsFor
  putStrLn ""
  putStrLn "Testing syntax errors..."
  testRunner testsErr
  putStrLn "-----------------------------------"

  
testsEmpty = [("", Val EmptyCell)]
  
testsNum = [ ("123", Val (Number 123))
           , ("1", Val (Number 1))
           , ("-111  ", Val (Number (-111)))
           , (" -12.123", Val (Number (-12.123)))
           , (" 0.041 ", Val (Number 0.041))
           , ("  0  ", Val (Number 0))
           , ("-0", Val (Number 0))
           , ("0000.134", Val (Number (0.134)))
           , ("-0001.14", Val (Number (-1.14)))
           , (".123", Val (Number (0.123)))
           , ("12.", Val (Number 12))
           , ("-12.", Val (Number (-12))) ]

testsFor = [ ("=12", For (Formula "=12" (Left FNoCache) (Just [Code "12"])))
           , ("=sum [1..10] + 25", For (Formula "=sum [1..10] + 25" (Left FNoCache) (Just [Code "sum [1..10] + 25"])))
           ]

testsErr = [ ("=", For (Formula "=" (Left FNoParse) Nothing))
           , (" = ", For (Formula " = " (Left FNoParse) Nothing))
           , ("=§a0 + 11", For (Formula "=§a0 + 11" (Left FNoParse) Nothing))
           , ("=§a0:A1", For (Formula "=§a0:A1" (Left FNoParse) Nothing))
           , ("=a0§", For (Formula "=a0§" (Left FNoParse) Nothing))
           , ("=12 + sumD §a11:bb12§", For (Formula "=12 + sumD §a11:bb12§" (Left FNoParse) Nothing))
           , ("=11 + product §a1::a2§", For (Formula "=11 + product §a1::a2§" (Left FNoParse) Nothing))
           , ("=§a1:2§", For (Formula "=§a1:2§" (Left FNoParse) Nothing)) ]

testRunner :: [(String, Cell)] -> IO ()
testRunner xs = do
  let failed = filter (\(s,c) -> rep s /= c) xs
      (flen, alen) = (length failed, length xs)
  putStrLn $ show (alen-flen) ++ '/' : show (alen) ++ " tests passed"
  if flen == 0
    then putStrLn "all tests passed"
    else forM_ failed $ \(s,c) -> do
           putStrLn $ "failed test: " ++ show s
           putStrLn $ "\tExpected: " ++ show c
           putStrLn $ "\tResult: " ++ show (rep s)
