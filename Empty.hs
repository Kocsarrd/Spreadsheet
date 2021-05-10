{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Empty ((€), sumD, module Data.Maybe) where


import Control.Monad
import Data.Maybe

infix 1 €
(€) :: ([a] -> b) -> a -> [Maybe a] -> b
f € b = f . map (maybe b id)

-- functions from Lists/NumberLists to b
type LFun a b = [Maybe a] -> b
type NLFun a b = Num a => [Maybe a] -> b

getFromJusts = map fromJust . filter isJust

onJusts :: ([a] -> b) -> LFun a b
onJusts f = f . map fromJust . filter isJust

-- re-exports from Prelude
-- this is purely boilerplate
-- type signatures need to be given explicitly

foldMapD :: Monoid m => (a -> m) -> LFun a m
foldMapD f = foldMap (maybe mempty f)  

elemD :: Eq a => a -> LFun a Bool
elemD = elem . Just

maximumD :: Ord a => LFun a a
maximumD = maximum . getFromJusts

minimumD :: Ord a => LFun a a
minimumD = minimum . getFromJusts

sumD :: NLFun a a
sumD = sum € 0

productD :: NLFun a a
productD = product € 1

notElemD :: Eq a => a -> LFun a Bool
notElemD = notElem . Just

headD :: LFun a a
headD = head € error "headD: element is invalid"

lastD :: LFun a a
lastD = last € error "lastD: element is invalid"

(!!!) :: LFun a (Int -> a)
(!!!) = (!!) € error "(!!!): element is invalid"

nullD :: LFun a Bool
nullD = null € undefined

lengthD :: LFun a Int
lengthD = length € undefined

anyD :: (a -> Bool) -> LFun a Bool
anyD p = any p . getFromJusts

allD :: (a -> Bool) -> LFun a Bool
allD p = all (maybe False p)

-- other functions
count :: (a -> Bool) -> [a] -> Int 
count p = foldr (\x -> if p x then (+1) else id) 0

countD :: (a -> Bool) -> NLFun a Int
countD p = foldr (\x -> if p x then (+1) else id) 0 . getFromJusts
