module Internal.Comms where

import Internal.Types
  ( Header(..)
  , Target(..)
  , Change
  , Resolve(..)
  , Resolves
  , CardID (..)
  )

import Data.List (sort)
import Text.Read (readMaybe)
import Control.Monad (liftM)
import Control.Concurrent.Chan (Chan, readChan, writeChan)

type Comm a = Chan String -> a -> IO a

requestReorder :: Comm [Header]
requestReorder _ [] = return []
requestReorder ch hdrs = do
  writeChan ch . (++) "ordr: " . show $ hdrs
  response <- readChan ch
  case reorder hdrs =<< readMaybe response of
    Nothing -> requestReorder ch hdrs
    (Just hdrs') -> return hdrs'

-- Will reorder the elements of a list if the order is unique and
-- the length is the same
reorder :: [a] -> [Int] -> Maybe [a]
reorder elems idxs
  | uniqueSpan idxs && (length idxs == length elems)
    = Just . foldr (reorder' elems) [] . map (flip (-) 1) $ idxs
  | otherwise = Nothing

reorder' :: [a] -> Int -> [a] -> [a]
reorder' elems idx = (:) (elems !! idx)

uniqueSpan :: [Int] -> Bool
uniqueSpan idxs = (==) [1..l] . sort $ idxs
  where l = length idxs



requestTargets :: Comm Header
requestTargets ch hdr@(Assigned cID aID grd trgts)
  = liftM (Targeted cID aID grd)
  . foldr f (return []) $ trgts
    where f x = (=<<) (requestTarget ch hdr x)
requestTargets _ hdr = return hdr


requestTarget :: Chan String -> Header -> (Resolve, Target)
                  -> [(Resolve, Maybe CardID)] -> IO [(Resolve, Maybe CardID)]
requestTarget ch hdr (rslv, trgt) acc
  = case trgt of
      Void -> return $ (rslv, Nothing) : acc
      (Given cID) -> return $ (rslv, Just cID) : acc
      (Inquire rng) -> do
        writeChan ch . (++) "trgt: " $ show hdr ++ " " ++ show rng
        response <- readChan ch
        case validTarget rng =<< readMaybe response of
          Nothing -> requestTarget ch hdr (rslv, trgt) acc
          (Just cID) -> return $ (rslv, Just . CardID $ cID) : acc
      (Random rng) -> do
        writeChan ch . (++) "rand: " $ show hdr ++ " " ++ show rng
        response <- readChan ch
        case validTarget rng =<< readMaybe response of
          Nothing -> requestTarget ch hdr (rslv, trgt) acc
          (Just cID) -> return $ (rslv, Just . CardID $ cID) : acc

validTarget :: [CardID] -> Int -> Maybe Int
validTarget xs x = case elem x . map cardID $ xs of
                     True -> Just x
                     False -> Nothing
