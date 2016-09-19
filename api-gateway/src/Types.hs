module Types
    ( Self (..)
    ) where

import Network.Nats (Nats)

-- | Context data for the API runtime.
data Self = Self
    { staticDir :: !FilePath
    , nats      :: !Nats
    }
