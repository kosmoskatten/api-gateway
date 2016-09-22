{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

-- | Entry module where the main APIs, 'CsimAPI' and 'API', are defined.
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

-- | The top level and combined API.
type API = CsimAPI :<|> SwaggerAPI :<|> StaticAPI

-- | The CSIM API, combined by the various sub APIs for CSIM.
type CsimAPI = MmeV1API

-- | API type for serving the Swagger file for 'CsimAPI'.
type SwaggerAPI = "api" :> "swagger.json" :> Get '[JSON] Swagger

-- | API type for the static file serving service.
type StaticAPI = Raw

-- | The application handling the 'API'.
app :: Self -> Application
app = serve apiProxy . apiService

-- | A prettified Swagger output 'CsimAPI'.
prettySwagger :: ByteString
prettySwagger = encodePretty csimSwagger

apiProxy :: Proxy API
apiProxy = Proxy

csimProxy :: Proxy CsimAPI
csimProxy = Proxy

-- | Service to provide the 'CsimAPI'.
csimService :: Self -> Server CsimAPI
csimService self = mmeV1Service self

-- | Service to provide the full 'API'.
apiService :: Self -> Server API
apiService self@Self {..}
      -- CSIM.
    = csimService self

      -- Swagger.
 :<|> return csimSwagger

    -- Static files.
 :<|> serveDirectory staticDir

-- | Generate Swagger for 'CsimAPI'.
csimSwagger :: Swagger
csimSwagger = toSwagger csimProxy
    & info.title .~ "CSIM REST API"
    & info.version .~ "1.0"
    & info.description ?~ "REST API to provide access to CSIM services."
    & info.license ?~ "Copyright (c) Ericsson, 2016"
