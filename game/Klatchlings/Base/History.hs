module Base.History
  ( History
  , Page(..)
  , Alteration(..)
  , show
  , begin
  , current
  , previous
  , entire
  , record
  , write
  , searchUntil
  ) where

import Internal.Types(History(..), Page(..), Alteration(..))

instance Show History where
  show (History (cur, past)) = show cur ++ show past

begin :: History
begin = History ([], [])

current :: History -> [Page]
current (History (cur, _)) = cur

previous :: History -> [Page]
previous (History (_, prev)) = prev 

entire :: History -> [Page]
entire (History (cur, prev)) = cur ++ prev

record :: Page -> History -> History
record pg (History (cur, prev)) = History (pg : cur, prev)

write :: History -> History
write (History (cur, prev)) = History ([], cur ++ prev)

searchUntil :: (Page -> Bool) -> History -> [Page]
searchUntil cond (History (cur, prev))
  = case searchUntil' cond cur [] of
      (False, pgs) -> snd . searchUntil' cond prev $ pgs
      (True, pgs) -> pgs

searchUntil' :: (Page -> Bool) -> [Page] -> [Page] -> (Bool, [Page])
searchUntil' _ [] acc = (False, acc)
searchUntil' cond (pg:pgs) acc
  = if cond pg
       then (True, acc)
       else searchUntil' cond pgs $ pg : acc

