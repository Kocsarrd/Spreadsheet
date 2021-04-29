module Test.Spreadsheet.Interface where

import Control.Monad
import Data.Graph.Inductive.Graph
import Data.List (sort)

import Spreadsheet.Interface
import Spreadsheet.Types

runSpreadsheetInterfaceTests :: IO ()
runSpreadsheetInterfaceTests = do
  putStrLn "-----------------------------------"
  putStrLn "Spreadsheet.Interface -- setCellState function"
  putStrLn ""
  mapM_ testRunner [setSimple, refsNoCycle, tryCycle1, tryCycle2, tryCycle4]
  putStrLn "-----------------------------------"

setSimple :: TestCase
setSimple = ("setSimple",
             [ ("setCellState 0 \"12\"", setCellState 0 "12",
                [ ("Graph has one node", nodeCount 1)
                , ("cell text is \"12.0\"", textCheck 0 "12.0")
                ]
               )
             , ("setCellState 1 \"=sum [1..10]\"", setCellState 1 "=sum [1..10]",
                [ ("Graph has two nodes", nodeCount 2)
                , ("Graph has no edges", edgeCount 0)
                ]
               )  
             , ("setCellState 0 \"11\"", setCellState 0 "11",
                [ ("Graph has two nodes", nodeCount 2)
                , ("cell text is \"11.0\"", textCheck 0 "11.0")
                ]
               )
             ]
            )

refsNoCycle :: TestCase
refsNoCycle = ("refsNoCycle",
               [ ("setCellState 0 \"12\"", setCellState 0 "12", []) 
               , ("setCellState 2 \"=§a0§\"", setCellState 2 "=§a0§",
                  [ ("cell text is \"FNoCache\"", textCheck 2 "FNoCache")
                  , ("graph has one edge", edgeCount 1)
                  , ("the edge goes from 0 to 2", preCheck 2 [0])
                  ]
                 )
               , ("setCellState 10 \"=sumD §a0:a2§\"", setCellState 10 "=sumD §a0:a2§",
                  [ ("graph has 4 nodes - node was added for 5 too", nodeCount 4)
                  , ("graph has 4 edges", edgeCount 4)
                  , ("there are edges: (0,10),(2,10),(5,10)", preCheck 10 [0,2,5])
                  ]
                 )
               ]
              )

tryCycle1 :: TestCase
tryCycle1 = ("tryCycle1",
             [ ("setCellState 0 \"=§a0\"", setCellState 0 "=§a0§",
                [ ("graph has one node", nodeCount 1)
                , ("graph has no edges", edgeCount 0)
                , ("cell text is \"FCycleRefError\"", textCheck 0 "FCycleRefError")
                ]
               )
             , ("setCellState 12 \"=sumD §b1:b4§\"", setCellState 12 "=sumD §b1:b4§",
                [ ("graph has 2 nodes", nodeCount 2)
                , ("graph has no edges", edgeCount 0)
                , ("cell text of 12 is \"FCycleRefError\"", textCheck 12 "FCycleRefError")
                ]
               )  
             ]
            )

tryCycle2 :: TestCase
tryCycle2 = ("tryCycle2",
             [ ("setCellState 0 \"12\"", setCellState 0 "12", [])
             , ("setCellState 1 \"=§a0\"", setCellState 1 "=§a0§", [])
             , ("setCellState 0 \"=§b0\"", setCellState 0 "=§b0§",
                [ ("graph has one edge", edgeCount 1)
                , ("edge goes from 0 to 1", preCheck 1 [0])
                , ("0's text is \"FCycleRefError\"", textCheck 0 "FCycleRefError")
                , ("1's text is \"FNoCache\"", textCheck 1 "FNoCache")
                ]
               )
             ]
            )

tryCycle4 :: TestCase
tryCycle4 = ("tryCycle2",
             [ ("setCellState 0 \"12\"", setCellState 0 "12", [])
             , ("setCellState 1 \"=§a0\"", setCellState 1 "=§a0§", [])
             , ("setCellState 4 \"=§b0\"", setCellState 4 "=§b0§", [])
             , ("setCellState 9 \"=§c0§+§c1§\"", setCellState 9 "=§c0§+§c1§",
                [ ("graph has 5 nodes", nodeCount 5)
                , ("graph has 4 edges", edgeCount 4)
                ])
             , ("setCellState 0 \"=§d0§+§d2§\"", setCellState 0 "=§d0§+§d2§",
                [ ("graph has 5 nodes", nodeCount 5)
                , ("graph has 4 edges", edgeCount 4)
                , ("0's text is \"FCycleRefError\"", textCheck 0 "FCycleRefError")
                ]
               )
             ]
            )

type TestCase = (String, [(String, Spreadsheet -> Spreadsheet, [(String, Spreadsheet -> Bool)])])

testRunner :: TestCase -> IO ()
testRunner (n,ts) = putStrLn n >> foldM_ go emptySpreadsheet ts where
  go ss (fname, f, ts) = do
    putStrLn $ '\t' : fname
    let ss' = f ss
    unless (checkInvariant ss') (putStrLn "\tinvariant broken")
    forM_ ts $ \(tname, t) ->
      putStrLn $ "\t\t" ++ tname ++ if t ss' then " - success" else " - failure"
    pure ss'  
    
nodeCount :: Int -> Spreadsheet -> Bool
nodeCount n = (==n).noNodes._sheet

edgeCount :: Int -> Spreadsheet -> Bool
edgeCount n = (==n).length.labEdges._sheet

checkInvariant :: Spreadsheet -> Bool
checkInvariant (SS ss _ _) = isEmpty $ labfilter (not.check) ss
  where
    check :: Cell -> Bool
    check (Val _) = True
    check (For (Formula _ (Left FNoParse) Nothing)) = True
    check (For (Formula _ (Left FCycleRefError) Nothing)) = True
    check (For (Formula _ (Left FNoCache) (Just _))) = True
    check (For (Formula _ (Left FListTypeError) (Just _))) = True
    check (For (Formula _ (Left FMissingDepError) (Just _))) = True
    check (For (Formula _ (Left FGhciError) (Just _))) = True
    check (For (Formula _ (Left FTimeoutError) (Just _))) = True
    check (For (Formula _ (Right cell') (Just _))) = True
    check _ = False

preCheck :: Node -> [Node] -> Spreadsheet -> Bool
preCheck n ns ss = sort (pre (_sheet ss) n) == ns 

textCheck :: Node -> String -> Spreadsheet -> Bool
textCheck n str = (==str).getCellText n
