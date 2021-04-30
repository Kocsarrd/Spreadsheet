module Test.Eval.CodeGeneration (runEvalCodeGenerationTests)where

import Control.Monad

import Eval.CodeGeneration
import Spreadsheet.Interface
import Spreadsheet.Types 

runEvalCodeGenerationTests :: IO ()
runEvalCodeGenerationTests = do
  putStrLn "-----------------------------------"
  putStrLn "Eval.CodeGeneration -- generateCode function"
  putStrLn ""
  mapM_ testRunner [valNoDep]
  putStrLn "-----------------------------------"


valNoDep :: TestCase
valNoDep = ("a0->\"12\",b0->\"alma\"", ss,
            [ (0, Right (["v0 = Just (12)"],[]))
            , (1, Right (["v1 = Just \"alma\""],[]))
            , (6, Right (["v6 = Nothing"],[]))
            ]) where
  ss = emptySpreadsheet
       & setCellState 0 "12"
       & setCellState 1 "alma"

type CodeGenResult = Either GenError ([String],[(String,CellID)])
type TestCase = (String, Spreadsheet, [(CellID, CodeGenResult)])

testRunner :: TestCase -> IO ()
testRunner (name, ss, tests) = putStrLn name >> (forM_ tests $ \(i,r) -> do
  putStrLn $ '\t' : show i ++ " --> " ++ show r
  putStrLn $ if generateCode ss i == r then "\tsuccess" else "\tfailure")

(&) = flip ($)
