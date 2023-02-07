module Internal.Misc where

import qualified Data.Map as Map (Map, keys)

getNextKey :: (k -> Int) -> Map.Map k a -> Int
getNextKey f = (+) 1 . maximum' . map f . Map.keys

maximum' :: [Int] -> Int
maximum' [] = 0
maximum' xs = maximum xs
