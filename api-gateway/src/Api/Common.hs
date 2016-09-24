{-# LANGUAGE OverloadedStrings #-}

-- | Common data structures, and common functionality for the API modules.
module Api.Common
    ( URL
    , concatURL
    , tmoRequest
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.Nats (Msg)
import Servant (Handler, err504, throwError)
import System.Timeout (timeout)

import qualified Data.Text as Text

import Types (TmoSec, toUsec)

-- | Type alias for URLs.
type URL = Text

-- | Concatenate the URL fragments to an URL.
concatURL :: [URL] -> URL
concatURL = Text.intercalate "/"

-- | Perform a Nats request, or any IO action with the type IO Msg. If the
-- action not is completed within the timeout duration a 504/Gateway timeout
-- is thrown.
tmoRequest :: TmoSec -> IO Msg -> (Msg -> Handler a) -> Handler a
tmoRequest tmo action handler = do
    result <- liftIO $ timeout (toUsec tmo) action
    maybe (throwError err504) handler result
