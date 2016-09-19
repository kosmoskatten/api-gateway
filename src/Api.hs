{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

-- | Entry module where the 'CsimAPI' is defined.
module Api
    ( CsimAPI
    , app
    ) where

import Servant

import Types (Self (..))

-- | Csim API type ...
type CsimAPI =
    "foo" :> Get '[JSON] [Int]
    :<|> StaticAPI

-- | API type for the static file serving service.
type StaticAPI = Raw

-- | The application handling the 'CsimAPI'.
app :: Self -> Application
app = serve apiProxy . apiRouter

apiProxy :: Proxy CsimAPI
apiProxy = Proxy

-- | Handlers for all routes. The routes must be in the same order as
-- in the 'CsimAPI'.
apiRouter :: Self -> Server CsimAPI
apiRouter Self {..} =
         return [1]
    :<|> serveDirectory staticDir
