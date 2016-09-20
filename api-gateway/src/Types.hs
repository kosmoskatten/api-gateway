module Types
    ( Self (..)
    , TmoSec (..)
    , toUsec
    ) where

import Network.Nats (Nats)

-- | Context data for the API runtime.
data Self = Self
    { staticDir :: !FilePath
    , nats      :: !Nats
    , tmo       :: !TmoSec
    }

-- | A type representing a timeout duration expressed in seconds.
newtype TmoSec = TmoSec Int
    deriving Show

-- | Convert a 'Tmo' value to a values understood by 'System.Timeout.timeout'.
toUsec :: TmoSec -> Int
toUsec (TmoSec s) = s * 1000000
