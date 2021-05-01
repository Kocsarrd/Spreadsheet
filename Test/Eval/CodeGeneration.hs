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
  mapM_ testRunner [valNoDep, forNoDep, linGraph]
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

forNoDep :: TestCase
forNoDep = ("a0->\"1\", b0->\"2\", a1->\"=§a0§+§b0§\", a2->\"=sum [1..10]\"", ss,
            [ (2, Right (["v0 = Just (1)", "v1 = Just (2)"], [("v2 = Just $ fromJust v0+fromJust v1",2)]))
            , (5, Right ([],[("v5 = Just $ sum [1..10]",5)]))
            ]) where
  ss = emptySpreadsheet
       & setCellState 0 "1"
       & setCellState 1 "2"
       & setCellState 2 "=§a0§+§b0§"
       & setCellState 5 "=sum [1..10]"

-- some outer dependencies are added twice
linGraph :: TestCase
linGraph = ("a0->\"12\", a1->\"=§a0§ + 2\", a2->\"=3*§A1§\", a3->\"=§a2§\"", ss,
              [ (0, Right (["v0 = Just (12)"], [ ("v2 = Just $ fromJust v0 + 2",2)
                                              , ("v5 = Just $ 3*fromJust v2",5)
                                              , ("v10 = Just $ fromJust v5",10)
                                              ]))
              , (2, Right (["v0 = Just (12)"], [ ("v2 = Just $ fromJust v0 + 2",2)
                                              , ("v5 = Just $ 3*fromJust v2",5)
                                              , ("v10 = Just $ fromJust v5",10)
                                              ]))
              ]) where
  ss = emptySpreadsheet
       & setCellState 0 "12"
       & setCellState 2 "=§a0§ + 2"
       & setCellState 5 "=3*§a1§"
       & setCellState 10 "=§a2§"

type CodeGenResult = Either GenError ([String],[(String,CellID)])
type TestCase = (String, Spreadsheet, [(CellID, CodeGenResult)])

testRunner :: TestCase -> IO ()
testRunner (name, ss, tests) = putStrLn name >> (forM_ tests $ \(i,r) -> do
  putStrLn $ '\t' : show i ++ " --> " ++ show r
  putStrLn $ if generateCode ss i == r then "\tsuccess" else "\tfailure")

(&) = flip ($)
