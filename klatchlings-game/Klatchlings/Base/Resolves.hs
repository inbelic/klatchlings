module Base.Resolves
  ( Resolve(..)
  , independent
  , advancePhase
  , moveZone
  , orSetUnactive
  ) where

import Internal.Types
  ( Resolve(..)
  , Resolves
  )

import Internal.Misc (maximum')
import Base.Fields
import Base.GameState
import Base.Card

import qualified Data.Map as Map
  ( fromList
  )

independent :: Change -> Resolve
independent chng = Resolve $ \_ _ _ -> chng

advancePhase :: Resolve
advancePhase
  = Resolve $ \_ _ gs ->
    case toEnum . retreive (CardID 0) (Attr Phase) . getCS $ gs of
      Start -> set Phase (fromEnum Morning)
      Night -> set Phase (fromEnum Morning)
      p -> set Phase (fromEnum $ succ p)

moveZone :: Zone -> Resolve
moveZone z = Resolve $ \_ tcID gs ->
  let owner = retreive tcID (Attr Owner) . getCS $ gs
      ownersZone
        = within 
        . refine (Attr Owner) ((==) owner)
        . refine (Attr Zone) ((==) TopDeck . toEnum)
        . getCS $ gs
      posn = (+ 1) . maximum' . map snd
           . orderBy (Attr Position) ownersZone . getCS $ gs
   in set Zone (fromEnum z) . fst . set Position posn

orSetUnactive :: Resolve -> Resolve
orSetUnactive (Resolve r) = Resolve rslv
  where rslv _ (CardID 0) _ = set ActiveFlag 0
        rslv cID tcID gs = r cID tcID gs
