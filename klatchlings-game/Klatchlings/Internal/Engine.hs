module Internal.Engine where

import Card
import History
import Internal.Types
import Internal.Comms

peek :: Game -> GameState
peek (Game stck hist crds)
  = GS stck hist . view $ crds

resolveStack :: Comm Game
resolveStack ch g@(Game stck hist crds) = do
  let gs = peek g
      hdrs = headers gs crds
  hdrs' <- (=<<) (sequence . map (requestTargets ch))
         $ requestReorder ch hdrs
  let hist' = write hist
      stck' = hdrs' ++ stck
      curEmpty = null . current $ hist
  resolveTrigger curEmpty ch $ Game stck' hist' crds

resolveTrigger :: Bool -> Comm Game
resolveTrigger curEmpty ch game@(Game [] _ _)
  | curEmpty = return game
  | otherwise = resolveStack ch game
resolveTrigger _ ch (Game (hdr : stck') hist crds)
  = let game' = Game stck' hist crds
     in case hdr of
          (Targeted cID grd trgts) ->
            resolveTargeted cID grd trgts ch game'
          (Unassigned cID grd trgts chngs) ->
            resolveUnassigned cID grd trgts chngs ch game'

resolveTargeted :: CardID -> Guard -> [(Change, CardID)] -> Comm Game
resolveTargeted cID grd trgts ch g@(Game stck hist crds)
  = resolveStack ch . Game stck hist' $ crds'
    where
      gs = peek g
      (hist', crds') = foldr (resolveChange cID gs grd) (hist, crds) trgts

resolveChange :: CardID -> GameState -> Guard -> (Change, CardID)
                  -> (History, Cards) -> (History, Cards)
resolveChange cID gs grd (chng, tcID) (hist, crds)
  | not $ guard grd cID tcID gs = (hist, crds)
  | otherwise = case applyChange chng tcID crds of
                  Nothing -> (hist, crds)
                  (Just (alt, crds')) ->
                    let pg = Page cID tcID alt
                     in (record pg hist, crds')

resolveUnassigned :: CardID -> Guard -> Targets -> Changes -> Comm Game
resolveUnassigned cID grd trgts chngs ch game@(Game stck hist crds) = do
  hdr' <- requestTargets ch . Assigned cID grd
        . targetChanges (peek game) cID chngs $ trgts
  resolveTrigger False ch (Game (hdr' : stck) hist crds)
