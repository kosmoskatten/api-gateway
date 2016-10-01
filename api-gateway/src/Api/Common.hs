{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common data structures, and common functionality for the API modules.
module Api.Common
    ( HasStatus (..)
    , URL
    , StatusCode
    , Status (..)
    , actOnStatus
    , concatTopic
    , concatURL
    , csimRequest
    , csimRequestJSON
    , tmoRequest
    , translateErrCode
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Nats (Msg, Topic, jsonPayload, request, requestJson)
import Servant ( Handler, throwError, err400, err404
               , err409, err415, err502, err504
               )
import System.Timeout (timeout)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import Types (Self (..), TmoSec, toUsec)

class HasStatus a where
    statusCode :: a -> StatusCode

-- | Type alias for URLs.
type URL = Text

type StatusCode = Int

-- | Generic status indicator reply from a CSIM service.
data Status = Status
    { status :: !StatusCode
    } deriving (Generic, Show, FromJSON, ToJSON)

instance HasStatus Status where
    statusCode = status

-- | Concat the 'Topic' fragments to a 'Topic'. The character (.) is
-- inserted in between the fragments.
concatTopic :: [Topic] -> Topic
concatTopic = BS.intercalate "."

actOnStatus :: HasStatus a => StatusCode -> (a -> r) -> a -> Handler r
actOnStatus sc action msg
    | statusCode msg == sc = return $ action msg
    | otherwise            = translateErrCode $ statusCode msg

-- | Concatenate the 'URL' fragments to an 'URL'. The character (/) is
-- inserted in between the fragments.
concatURL :: [URL] -> URL
concatURL = Text.intercalate "/"

-- | Make a request to a CSIM service, but without any request payload.
csimRequest :: FromJSON a => Self -> Topic -> (a -> Handler r) -> Handler r
csimRequest self topic handleReply =
    tmoRequest (tmo self) (request (nats self) topic "") $ \msg ->
        maybe (throwError err502) handleReply (jsonPayload msg)

-- | Make a request to a CSIM service, with a JSON request payload.
csimRequestJSON :: (FromJSON a, ToJSON b) => Self -> Topic -> b
                -> (a -> Handler r) -> Handler r
csimRequestJSON self topic payload handleReply =
    tmoRequest (tmo self) (requestJson (nats self) topic payload) $ \msg ->
        maybe (throwError err502) handleReply (jsonPayload msg)

-- | Perform a Nats request, or any IO action with the type IO Msg. If the
-- action not is completed within the timeout duration a 504/Gateway timeout
-- is thrown.
tmoRequest :: TmoSec -> IO Msg -> (Msg -> Handler a) -> Handler a
tmoRequest tmo action handler = do
    result <- liftIO $ timeout (toUsec tmo) action
    maybe (throwError err504) handler result

-- | Translate error codes - coming as status replies from some service -
-- to Servant errors. If no translation can be made the 502/Bad gateway will
-- be used.
translateErrCode :: Int -> Handler a
translateErrCode code =
    case code of
        400 -> throwError err400
        404 -> throwError err404
        409 -> throwError err409
        415 -> throwError err415
        _   -> throwError err502
