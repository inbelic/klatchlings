module Base.History
  ( History
  , show
  , begin
  , current
  , previous
  , entire
  , record
  , write
  ) where

import Internal.Types(History(..), Page)

instance Show History where
  show (History (cur, past)) = show cur ++ show past

begin :: History
begin = History ([], [])

current :: History -> [Page]
current (History (cur, _)) = cur

previous :: History -> [Page]
previous (History (_, prev)) = prev 

entire :: History -> [Page]
entire (History (cur, past)) = cur ++ past

record :: Page -> History -> History
record pg (History (cur, past)) = History (pg : cur, past)

write :: History -> History
write (History (cur, past)) = History ([], cur ++ past)
