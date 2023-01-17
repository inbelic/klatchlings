module Internal.Comms where

import Internal.Types (Header(..))
import Control.Concurrent.Chan

type Comm a = Chan String -> a -> IO a

requestReorder :: Comm [Header]
requestReorder = undefined

requestTargets :: Comm Header
requestTargets ch hdr = undefined
