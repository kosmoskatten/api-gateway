module Types
    ( Self (..)
    ) where

-- | Context data for the API runtime.
data Self = Self
    { staticDir :: !FilePath
    }
