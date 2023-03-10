module Internal.Engine where

import Base.Card (view, headers, applyResolve, targetResolves)
import Base.GameState (peek)
import Base.History (write, current, record)
import Internal.Types
import Internal.Comms (Comm, requestReorder, requestTargets, displayState)
import Internal.Misc (getNextKey)

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
          (Targeted cID aID grd trgts) ->
            resolveTargeted cID aID grd trgts ch game'
          (Unassigned lbl cID aID grd trgts rslvs) ->
            resolveUnassigned lbl cID aID grd trgts rslvs ch game'

resolveTargeted :: CardID -> AbilityID -> Guard -> [(Resolve, Maybe CardID)] 
                -> Comm Game
resolveTargeted cID aID grd trgts ch g@(Game stck hist crds)
  = do
    displayState cs ch . peek $ g'
    resolveStack ch g'
    where
      g' = Game stck hist' crds'
      gs@(GameState _ _ cs) = peek g
      (hist', crds') = foldr (resolveResolve cID aID gs grd . fillVoid crds)
                        (hist, crds) trgts
      fillVoid :: Cards -> (Resolve, Maybe CardID) -> (Resolve, CardID)
      fillVoid _ (rslv, Just cID) = (rslv, cID)
      fillVoid crds (rslv, Nothing) = (rslv, CardID . getNextKey cardID $ crds)

resolveResolve :: CardID -> AbilityID -> GameState -> Guard -> (Resolve, CardID)
            -> (History, Cards) -> (History, Cards)
resolveResolve cID aID gs grd (rslv, tcID) (hist, crds)
  | not $ guard grd cID tcID gs = (hist, crds)
  | otherwise =
    let (alt, crds') = applyResolve rslv cID tcID gs crds
        pg = Page cID tcID alt
     in (record pg hist, crds')

resolveUnassigned :: Liable -> CardID -> AbilityID -> Guard -> Targets
                      -> Resolves -> Comm Game
resolveUnassigned lbl cID aID grd trgts rslvs ch game@(Game stck hist crds) = do
  hdr' <- requestTargets ch . Assigned lbl cID aID grd
        . targetResolves (peek game) cID rslvs $ trgts
  resolveTrigger False ch (Game (hdr' : stck) hist crds)
