{-# LANGUAGE OverloadedStrings #-}

-- | Common data structures, and common functionality for the API modules.
module Api.Common
    ( URL
    , concatTopic
    , concatURL
    , tmoRequest
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.Nats (Msg, Topic)
import Servant (Handler, err504, throwError)
import System.Timeout (timeout)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import Types (TmoSec, toUsec)

-- | Type alias for URLs.
type URL = Text

-- | Concat the 'Topic' fragments to a 'Topic'. The character (.) is
-- inserted in between the fragments.
concatTopic :: [Topic] -> Topic
concatTopic = BS.intercalate "."

-- | Concatenate the 'URL' fragments to an 'URL'. The character (/) is
-- inserted in between the fragments.
concatURL :: [URL] -> URL
concatURL = Text.intercalate "/"

-- | Perform a Nats request, or any IO action with the type IO Msg. If the
-- action not is completed within the timeout duration a 504/Gateway timeout
-- is thrown.
tmoRequest :: TmoSec -> IO Msg -> (Msg -> Handler a) -> Handler a
tmoRequest tmo action handler = do
    result <- liftIO $ timeout (toUsec tmo) action
    maybe (throwError err504) handler result
