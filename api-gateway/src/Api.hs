{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

-- | Entry module where the 'CsimAPI' and it's API serving Application
-- 'csimAPI' is defined.
module Api
    ( api
    , prettySwagger
    ) where

import Control.Lens
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString)
import Data.Swagger
import Servant
import Servant.Swagger

import Types (Self (..))

import qualified Api.MmeV1 as MmeV1

-- | Csim API type ...
type CsimAPI =
    -- Endpoints for Mme (V1).
         MmeV1.ListMmes
    :<|> MmeV1.CreateMme
    :<|> MmeV1.DeleteMme
    :<|> MmeV1.GetIpConfig

    -- Endpoint to serve static files.
    :<|> StaticAPI

-- | API type for the static file serving service.
type StaticAPI = Raw

-- | The application handling the 'CsimAPI'.
api :: Self -> Application
api = serve apiProxy . apiRouter

-- | A prettified Swagger output for Csim's API.
prettySwagger :: ByteString
prettySwagger = encodePretty csimSwagger

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

csimSwagger :: Swagger
csimSwagger = toSwagger apiProxy
    & info.title .~ "CSIM API"
