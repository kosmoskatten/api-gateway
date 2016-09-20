module Common
    ( stayAlive
    , splitTopic
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import qualified Data.ByteString.Char8 as BS

import Network.Nats (Topic)

stayAlive :: IO ()
stayAlive =
    forever $ threadDelay 5000000

splitTopic :: Topic -> [Topic]
splitTopic = BS.split '.'
