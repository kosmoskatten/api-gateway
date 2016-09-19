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

import qualified Api.MmeV1 as MmeV1

-- | Csim API type ...
type CsimAPI =
    -- Endpoints for Mme (V1).
         MmeV1.ListMmes
    :<|> MmeV1.CreateMme
    :<|> MmeV1.DeleteMme
    :<|> MmeV1.GetIpConfig

    -- Endpoint for access to static files.
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
apiRouter self@Self {..} =
    -- Handlers for Mme (V1).
         MmeV1.listMmes self
    :<|> MmeV1.createMme self
    :<|> MmeV1.deleteMme self
    :<|> MmeV1.getIpConfig self

    -- Handler for static file serving.
    :<|> serveDirectory staticDir
