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
import Servant

import Api.Common ( HasStatus (..), URL, StatusCode, Status (..)
                  , actOnStatus, concatTopic, concatURL
                  , csimRequest, csimRequestJSON
                  )
import Types (Self)

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

      -- List all attributes for the referenced MME.
 :<|> "api" :> "v1" :> "mme" :> Capture "name" Text
                             :> Get '[JSON] [MmeAttributeDesc]

      -- Get the IP config attribute for the references MME.
 :<|> "api" :> "v1" :> "mme" :> Capture "name" Text :> "ip-config"
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
                toJSON MmeAttributeDesc { url = "/api/v1/mme/mme1/ip-config"
                                        , desc = "Fetch MME IP configuration"
                                        }

-- | List of MME names, with status indicator.
data MmeNameList = MmeNameList
    { status :: !StatusCode
    , names  :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

instance HasStatus MmeNameList where
    statusCode = status

-- | IP config payload, with status indicator.
data IpConfig = IpConfig
    { status :: !StatusCode
    , config :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

instance HasStatus IpConfig where
    statusCode = status

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
listMmes self =
    csimRequest self "app.v1.mme.listPcos" $ actOnStatus 200 handleReply
    where
        handleReply :: MmeNameList -> [MmeUrlRef]
        handleReply MmeNameList {..} =
            map (\n -> MmeUrlRef { url = concatURL [baseUrl, n] })
            (fromJust names)

-- | Create a new MME. References the app.v1.mme.createPco topic.
createMme :: Self -> MmeCtor -> Handler MmeUrlRef
createMme self mmeCtor@MmeCtor {..} =
    csimRequestJSON self "app.v1.mme.createPco" mmeCtor $
        actOnStatus 201 handleReply
    where
        handleReply :: Status -> MmeUrlRef
        handleReply _ = MmeUrlRef { url = concatURL [baseUrl, name] }

-- | Delete a MME. References the app.v1.mme.deletePco.* topic.
deleteMme :: Self -> Text -> Handler NoContent
deleteMme self name = do
    let topic' = concatTopic ["app.v1.mme.deletePco", cs name]
    csimRequest self topic' $ actOnStatus 200 handleReply
    where
        handleReply :: Status -> NoContent
        handleReply _ = NoContent

-- | List a MME's attributes. The MME must exist. References the
-- app.v1.mme.*.exist topic.
listAttributes :: Self -> Text -> Handler [MmeAttributeDesc]
listAttributes self name = do
    let topic' = concatTopic ["app.v1.mme", cs name, "exist"]
    csimRequest self topic' $ actOnStatus 200 handleReply
    where
        handleReply :: Status -> [MmeAttributeDesc]
        handleReply _ =
            [ MmeAttributeDesc
                { url = concatURL [baseUrl, name, "ip-config"]
                , desc = "Fetch MME IP configuration"
                }
            ]

-- | Get the IP config attribute for a MME. The MME must exist. References the
-- app.v1.mme.*.getIpConfig topic.
getIpConfig :: Self -> Text -> Handler [Text]
getIpConfig self name = do
    let topic' = concatTopic ["app.v1.mme", cs name, "getIpConfig"]
    csimRequest self topic' $ actOnStatus 200 handleReply
    where
        handleReply :: IpConfig -> [Text]
        handleReply IpConfig {..} = fromJust config

baseUrl :: URL
baseUrl = "/api/v1/mme"
