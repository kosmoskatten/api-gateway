-- | Common functionality for the API modules.
module Api.Common
    ( tmoRequest
    ) where

import Control.Monad.IO.Class (liftIO)
import Network.Nats (Msg)
import Servant
import System.Timeout (timeout)

import Types (TmoSec, toUsec)

-- | Perform a Nats request, or any IO action with the type IO Msg. If the
-- action not is completed within the timeout duration a 504/Gateway timeout
-- is thrown.
tmoRequest :: TmoSec -> IO Msg -> (Msg -> Handler a) -> Handler a
tmoRequest tmo action handler = do
    result <- liftIO $ timeout (toUsec tmo) action
    maybe (throwError err504) handler result
