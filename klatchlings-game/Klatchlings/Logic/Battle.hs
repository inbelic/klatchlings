module Logic.Battle
  ( constructStrikes
  , getSeiging
  ) where

import Base.Fields
import Base.GameState
import Base.History

constructStrikes :: CardState -> [(CardID, CardID, Int)]
constructStrikes cs
  = computeStrikes p1Strks p2Blcks
  . computeStrikes p2Strks p1Blcks
  $ []
  where (p1Strks, p1Blcks)
          = foldr mkStructs (NullStrike, mkHeroBlock P1 cs)
          . map (collector . fst)
          . flip (orderBy (Attr Position)) cs
          . getOwnersZone P1 Battlefield
          $ cs

        (p2Strks, p2Blcks)
          = foldr mkStructs (NullStrike, mkHeroBlock P2 cs)
          . map (collector . fst)
          . flip (orderBy (Attr Position)) cs
          . getOwnersZone P2 Battlefield
          $ cs

        collector cID = ( cID
                        , retreive cID (Stat Power) cs
                        , retreive cID (Stat Toughness) cs
                        )


data Strikes = NullStrike | Strike CardID Int Strikes
data Blocks = NullBlock | Block CardID Int Blocks

mkHeroBlock :: Owner -> CardState -> Blocks
mkHeroBlock o cs = Block hID (retreive hID (Stat Toughness) cs) NullBlock
  where hID = getHero o cs

mkStructs :: (CardID, Int, Int) -> (Strikes, Blocks) -> (Strikes, Blocks)
mkStructs (cID, pwr, tgh) (strks, blcks)
  = (Strike cID pwr strks, Block cID tgh blcks)

computeStrikes :: Strikes -> Blocks
                    -> [(CardID, CardID, Int)] -> [(CardID, CardID, Int)]
computeStrikes NullStrike _ = id
computeStrikes _ NullBlock = id
computeStrikes (Strike scID remPwr nxtStrk) (Block bcID remTgh nxtBlck)
  = case compare remPwr remTgh of
      EQ -> computeStrikes nxtStrk nxtBlck
          . (:) (scID, bcID, remPwr)
      LT -> computeStrikes nxtStrk (Block bcID (remTgh - remPwr) nxtBlck)
          . (:) (scID, bcID, remPwr)
      GT -> computeStrikes (Strike scID (remPwr - remTgh) nxtStrk) nxtBlck
          . (:) (scID, bcID, remTgh)


getSeiging :: GameState -> Owner
getSeiging (GameState _ hist cs)
  = case compare p1Dmg p2Dmg of
      EQ -> curSeiging
      LT -> P2
      GT -> P1
  where
    p1Hero = getHero P1 cs
    p2Hero = getHero P2 cs
    curSeiging = toEnum . retreive (CardID 0) (Attr AttackFlag) $ cs
    strikes = searchUntil cond hist
    p1Dmg = sumDmg p2Hero strikes
    p2Dmg = sumDmg p1Hero strikes

    cond :: Page -> Bool
    cond (Page _ (CardID 0) (Set Phase p))
      = (==) (fromEnum Skirmish) p
    cond _ = False

    sumDmg :: CardID -> [Page] -> Int
    sumDmg cID = foldr (f cID) 0

    f cID (Page _ tcID (Shift (Stat Toughness) amt))
      | cID == tcID = (+) (abs amt)
      | otherwise = id
    f _ _ = id
