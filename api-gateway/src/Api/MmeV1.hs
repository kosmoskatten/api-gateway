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

import Api.Common (URL, concatURL, tmoRequest)
import Types (Self (..))

-- | The type specifying the interface's endpoints.
type MmeV1API
      -- List all registered MMEs.
    = "api" :> "v1" :> "mme" :> Get '[JSON] [MmeUrlRef]

      -- Create a new MME with the given name.
 :<|> "api" :> "v1" :> "mme" :> ReqBody '[JSON] MmeCtor
                             :> PostCreated '[JSON] MmeUrlRef

      -- Delete the referenced MME.
 :<|> "api" :> "v1" :> "mme" :> Capture "name" Text
                             :> DeleteNoContent '[JSON] NoContent

      -- List all attributes for the references MME.
 :<|> "api" :> "v1" :> "mme" :> Capture "name" Text
                             :> Get '[JSON] [MmeAttributeDesc]

      -- Get the IP config attribute for the references MME.
 :<|> "api" :> "v1" :> "mme" :> Capture "name" Text :> "ip_config"
                             :> Get '[JSON] [Text]

-- | JSON object to create a new MME.
data MmeCtor = MmeCtor
    { name :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'MmeCtor'
instance ToSchema MmeCtor where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "MME constructor object"
            & mapped.schema.example ?~ toJSON (MmeCtor "mme1")

-- | JSON object to construct a new MME.
data MmeUrlRef = MmeUrlRef
    { url :: !URL
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'MmeUrlRef'.
instance ToSchema MmeUrlRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object holding a resource URL"
            & mapped.schema.example ?~ toJSON (MmeUrlRef "/api/v1/mme/mme1")

-- | Describe the attributes provided by a MME.
data MmeAttributeDesc = MmeAttributeDesc
    { url  :: !URL
    , desc :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'MmeAttributeDesc'.
instance ToSchema MmeAttributeDesc where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "MME attribute"
            & mapped.schema.example ?~
                toJSON MmeAttributeDesc { url = "/api/v1/mme/mme1/ip_config"
                                        , desc = "Fetch MME IP configuration"
                                        }

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
 :<|> listAttributes self
 :<|> getIpConfig self

-- | List all MMEs. References the app.v1.mme.listPcos topic.
listMmes :: Self -> Handler [MmeUrlRef]
listMmes Self {..} =
    tmoRequest tmo (request nats "app.v1.mme.listPcos" "") $ \msg ->
        maybe (throwError err502) (return . map toMmeUrlRef) (jsonPayload msg)
    where
        toMmeUrlRef :: URL -> MmeUrlRef
        toMmeUrlRef u = MmeUrlRef { url = concatURL [baseUrl, u]}

-- | Create a new MME. References the app.v1.mme.createPco topic.
createMme :: Self -> MmeCtor -> Handler MmeUrlRef
createMme Self {..} MmeCtor {..} = do
    let topic' = "app.v1.mme.createPco." `mappend` cs name
    tmoRequest tmo (request nats topic' "") $ \msg ->
        maybe (throwError err502) handleStatus (jsonPayload msg)
    where
        handleStatus :: Status -> Handler MmeUrlRef
        handleStatus Status {..} =
            case status of
                201 -> return MmeUrlRef { url = concatURL [baseUrl, name] }
                409 -> throwError err409
                _   -> throwError err502

-- | Delete a MME. References the app.v1.mme.deletePco.* topic.
deleteMme :: Self -> Text -> Handler NoContent
deleteMme Self {..} name = do
    let topic' = "app.v1.mme.deletePco." `mappend` cs name
    tmoRequest tmo (request nats topic' "") $ \msg ->
        maybe (throwError err502) handleStatus (jsonPayload msg)
    where
        handleStatus :: Status -> Handler NoContent
        handleStatus Status {..} =
            case status of
                200 -> return NoContent
                404 -> throwError err404
                _   -> throwError err502

-- | List a MME's attributes. The MME must exist. References the
-- app.v1.mme.*.exist topic.
listAttributes :: Self -> Text -> Handler [MmeAttributeDesc]
listAttributes Self {..} name = do
    let topic' = "app.v1.mme." `mappend` cs name `mappend` ".exist"
    tmoRequest tmo (request nats topic' "") $ \msg ->
        maybe (throwError err502) handleStatus (jsonPayload msg)
    where
        handleStatus :: Status -> Handler [MmeAttributeDesc]
        handleStatus Status {..} =
            case status of
                200 -> return
                    [ MmeAttributeDesc
                        { url = concatURL [baseUrl, name, "ip_config"]
                        , desc = "Read the IP configuration from the MME"
                        }
                    ]
                404 -> throwError err404
                _   -> throwError err502

-- | Get the IP config attribute for a MME. The MME must exist. References the
-- app.v1.mme.*.getIpConfig topic.
getIpConfig :: Self -> Text -> Handler [Text]
getIpConfig Self {..} name = do
    let topic' = "app.v1.mme." `mappend` cs name `mappend` ".getIpConfig"
    tmoRequest tmo (request nats topic' "") $ \msg ->
        maybe (throwError err502) handleStatus (jsonPayload msg)
    where
        handleStatus :: IpConfig -> Handler [Text]
        handleStatus IpConfig {..} =
            case status of
                200 -> return $ fromJust config
                404 -> throwError err404
                _   -> throwError err502

baseUrl :: URL
baseUrl = "/api/v1/mme"
