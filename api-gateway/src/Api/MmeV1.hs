{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

-- | Module implementing version 1 of the MME REST interface.
module Api.MmeV1
    ( MmeV1API
    , mmeV1Service
    ) where

import Control.Lens ((&), (?~), mapped)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Maybe (fromJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Swagger ( ToSchema (..), genericDeclareNamedSchema
                    , defaultSchemaOptions, schema
                    , description, example
                    )
import GHC.Generics (Generic)
import Network.Nats
import Servant

import Api.Common (tmoRequest)
import Types (Self (..))

-- | The type specifying the interface's endpoints.
type MmeV1API
      -- List all registered MMEs.
    = "api" :> "v1" :> "mme" :> Get '[JSON] [MmeUrlRef]

      -- Create a new MME with the given name.
 :<|> "api" :> "v1" :> "mme" :> ReqBody '[JSON] NameRef
                             :> PostCreated '[JSON] MmeUrlRef

      -- Delete the referenced MME.
 :<|> "api" :> "v1" :> "mme" :> Capture "name" Text
                             :> DeleteNoContent '[JSON] NoContent

      -- Get the IP config for the referenced MME.
 :<|> "api" :> "v1" :> "mme" :> Capture "name" Text :> "ip_config"
                             :> Get '[JSON] [Text]

-- | JSON object with one member, the name of the MME to be created.
data NameRef = NameRef
    { name :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'NameRef'
instance ToSchema NameRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object naming a resource"
            & mapped.schema.example ?~ toJSON (NameRef "mme1")

-- | JSON object with one member, the url to a MME resource.
data MmeUrlRef = MmeUrlRef
    { url :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'MmeUrlRef'.
instance ToSchema MmeUrlRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object holding a resource URL"
            & mapped.schema.example ?~ toJSON (MmeUrlRef "/api/v1/mme/mme1")

-- | Status indicator from the MME component.
data Status = Status
    { status :: !Int
    } deriving (Generic, Show, FromJSON, ToJSON)

-- Status indicator, with payload, from the MME component.
data IpConfig = IpConfig
    { status :: !Int
    , config :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | The service implementing the 'MmeV1API'.
mmeV1Service :: Self -> Server MmeV1API
mmeV1Service self
    = listMmes self
 :<|> createMme self
 :<|> deleteMme self
 :<|> getIpConfig self

-- | Request the app.v1.mme service to list all its pco names. If the
-- request is timing out, a 504 (Gateway Timeout) is returned. If the
-- requested payload not can be decoded, a 502 (Bad Gateway) is returned.
-- If everything is ok, 200 Ok is retured with a list of 'UrlRef'.
listMmes :: Self -> Handler [MmeUrlRef]
listMmes Self {..} =
    tmoRequest tmo (request nats "app.v1.mme.listPcos" "") $ \msg ->
        maybe (throwError err502) (return . map toUrlRef) (jsonPayload msg)

-- | Request the app.v1.mme.createPco service to create a new pco with
-- the given name.
createMme :: Self -> NameRef -> Handler MmeUrlRef
createMme Self {..} NameRef {..} = do
    let topic' = "app.v1.mme.createPco." `mappend` (cs name)
    tmoRequest tmo (request nats topic' "") $ \msg ->
        maybe (throwError err502) handleStatus (jsonPayload msg)
    where
        handleStatus :: Status -> Handler MmeUrlRef
        handleStatus Status {..} =
            case status of
                201 -> return $ toUrlRef name
                409 -> throwError err409
                _   -> throwError err502

-- | Request the app.v1.mme.deletePco to delete the pco with the given name.
deleteMme :: Self -> Text -> Handler NoContent
deleteMme Self {..} name = do
    let topic' = "app.v1.mme.deletePco." `mappend` (cs name)
    tmoRequest tmo (request nats topic' "") $ \msg ->
        maybe (throwError err502) handleStatus (jsonPayload msg)
    where
        handleStatus :: Status -> Handler NoContent
        handleStatus Status {..} =
            case status of
                200 -> return NoContent
                404 -> throwError err404
                _   -> throwError err502

-- | Request the app.v1.mme.<name>.getIpConfig to send its IP config.
getIpConfig :: Self -> Text -> Handler [Text]
getIpConfig Self {..} name = do
    let topic' = "app.v1.mme." `mappend` (cs name) `mappend` ".getIpConfig"
    tmoRequest tmo (request nats topic' "") $ \msg ->
        maybe (throwError err502) handleStatus (jsonPayload msg)
    where
        handleStatus :: IpConfig -> Handler [Text]
        handleStatus IpConfig {..} =
            case status of
                200 -> return $ fromJust config
                404 -> throwError err404
                _   -> throwError err502

toUrlRef :: Text -> MmeUrlRef
toUrlRef mmeName =
    MmeUrlRef { url = "/api/v1/mme/" `mappend` mmeName }
