{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

-- | Entry module where the 'CsimAPI' and it's API serving Application
-- 'csimAPI' is defined.
module Api
    ( app
    , prettySwagger
    ) where

import Control.Lens
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString)
import Data.Swagger
import Servant
import Servant.Swagger

import Api.MmeV1 (MmeV1API, mmeV1Service)
import Types (Self (..))

-- | The top level, combined, API.
type API = CsimAPI :<|> SwaggerAPI :<|> StaticAPI

-- | The CSIM API, combined by the various sub APIs.
type CsimAPI = MmeV1API

type SwaggerAPI = "api" :> "swagger.json" :> Get '[JSON] Swagger

-- | API type for the static file serving service.
type StaticAPI = Raw

-- | The application handling the 'API'.
app :: Self -> Application
app = serve apiProxy . apiServer

-- | A prettified Swagger output for Csim's API.
prettySwagger :: ByteString
prettySwagger = encodePretty csimSwagger

apiProxy :: Proxy API
apiProxy = Proxy

csimProxy :: Proxy CsimAPI
csimProxy = Proxy

-- | Handlers for all routes. The routes must be in the same order as
-- in the 'CsimAPI'.
apiServer :: Self -> Server API
apiServer self@Self {..}
    -- Service for Mme (V1).
    = mmeV1Service self

 :<|> return csimSwagger

    -- Handler for static file serving.
 :<|> serveDirectory staticDir

csimSwagger :: Swagger
csimSwagger = toSwagger csimProxy
    & info.title .~ "CSIM API"
