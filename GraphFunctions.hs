module GraphFunctions where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query
import Data.Maybe (fromJust)
import Lens.Micro.Platform hiding ((&))

lookupNodeThen :: DynGraph gr => Node
               -> (Decomp gr a b -> gr a b)
               -> (Decomp gr a b -> gr a b)
               ->  gr a b -> gr a b
lookupNodeThen node ifFound ifNot gr = case match node gr of
  dc@(Just c, _) -> ifFound dc
  dc@_           -> ifNot dc

-- to be called in lookupNodeThen' first case
changeNodeLabBy :: DynGraph gr => (a -> a) -> Decomp gr a b -> gr a b
changeNodeLabBy f (Just c, cg) = over _3 f c & cg

changeNodeLab :: DynGraph gr => a -> Decomp gr a b -> gr a b
changeNodeLab = changeNodeLabBy . const 

-- every edge counts as one
lpLevel :: (DynGraph gr) => Node -> gr a b -> gr Int b
lpLevel v gr = lg
  where
    lg = foldl go (nmap (const 0) sg) vs 
    go gr' v' = foldr (go' $ fromJust (lab gr' v')) gr' (suc gr' v')
    go' l'  v' gr' = lookupNodeThen v' (changeNodeLabBy $ \l -> max l (l'+1))
                    (const gr') gr' 
    vs = topsort sg
    sg = subgraph (reachable v gr) gr
