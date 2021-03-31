module Empty ((€), sumD, fromJust) where

import Data.Maybe (fromJust)

infix 1 €
(€) :: ([a] -> b) -> a -> [Maybe a] -> b
f € b = f . map (maybe b id)


sumD :: Num a => [Maybe a] -> a
sumD = sum € 0
