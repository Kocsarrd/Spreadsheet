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
  mapM_ testRunner [valNoDep, forNoDep, linGraph, treeGraph, dagGraph]
  putStrLn "-----------------------------------"


valNoDep :: TestCase
valNoDep = ("a0->\"12\",b0->\"alma\"", ss,
            [ (0, Right (["v0 = Just (12)"],[]))
            , (1, Right (["v1 = Just \"alma\""],[]))
            , (6, Right (["v6 = Nothing"],[])) -- don't know for sure if this is an error
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

treeGraph :: TestCase
treeGraph = ("a0->\"12\",a1->\"11\", b0->\"=3*§a0§\", b1->\"=§b0§+2\", c1->\"=sumD §a0:a1§\"", ss,
             [ (0, Right (["v0 = Just (12)","v2 = Just (11)"],[ ("v1 = Just $ 3*fromJust v0",1)
                                                              , ("v6 = Just $ sumD [v0,v2]",6)
                                                              , ("v3 = Just $ fromJust v1+2",3)
                                                              ]))
             , (3, Right (["v1 = Just (36)"],[("v3 = Just $ fromJust v1+2",3)]))
             ]) where
  ss = emptySpreadsheet
       & setCellState 0 "12"
       & setCellState 2 "11"
       & setCellState 1 "=3*§a0§"
       & cacheCell 1 (Right "36")
       & setCellState 3 "=§b0§+2"
       & setCellState 6 "=sumD §a0:a1§"

dagGraph :: TestCase
dagGraph = ("a0->\"=sum [1..10]\",b0->->\"=§a0§+2\",c0->\"=sumD §a0:b0§\",a1->\"=3*§a0§\",\
            \d0->\"=3*§c0§\", e0->\"=§d0§\",a2->\"=§e0§*§a1§\"", ss,
            [ (0, Right ([],[ ("v0 = Just $ sum [1..10]",0)
                            , ("v1 = Just $ fromJust v0+2",1)
                            , ("v2 = Just $ 3*fromJust v0",2)
                            , ("v4 = Just $ sumD [v0,v1]",4)
                            , ("v9 = Just $ 3*fromJust v4",9)
                            , ("v16 = Just $ fromJust v9",16)
                            , ("v5 = Just $ fromJust v16*fromJust v2",5)
                            ]))
            --, 
            ]) where
  ss = emptySpreadsheet
       & setCellState 0 "=sum [1..10]"  -- a0
       & setCellState 1 "=§a0§+2"       -- b0
       & cacheCell 1 (Right "57")
       & cacheCell 0 (Right "55")
       & setCellState 4 "=sumD §a0:b0§" -- c0
       & setCellState 2 "=3*§a0§"       -- a1
       & setCellState 9 "=3*§c0§"       -- d0
       & setCellState 16 "=§d0§"        -- e0
       & setCellState 5 "=§e0§*§a1§"    -- a2


type CodeGenResult = Either GenError ([String],[(String,CellID)])
type TestCase = (String, Spreadsheet, [(CellID, CodeGenResult)])

-- could define pretty printing here
testRunner :: TestCase -> IO ()
testRunner (name, ss, tests) = putStrLn name >> (forM_ tests $ \(i,r) -> do
  putStrLn $ '\t' : show i ++ " --> " ++ show r
  putStrLn $ if generateCode ss i == r then "\tsuccess" else "\tfailure")

(&) = flip ($)
