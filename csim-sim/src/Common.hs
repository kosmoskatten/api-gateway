module Common
    ( stayAlive
    , splitTopic
    , ifReply
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import qualified Data.ByteString.Char8 as BS

import Network.Nats (Msg, Topic, replyTo)

stayAlive :: IO ()
stayAlive =
    forever $ threadDelay 5000000

splitTopic :: Topic -> [Topic]
splitTopic = BS.split '.'

ifReply :: Msg -> (Topic -> IO ()) -> IO ()
ifReply msg action = maybe (return ()) action (replyTo msg)
