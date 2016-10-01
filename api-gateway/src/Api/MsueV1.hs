{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

-- | Module implementing version 1 of the Msue REST API.
module Api.MsueV1
    ( MsueV1API
    , msueV1Service
    ) where

import Control.Lens ((&), (?~), mapped)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Maybe (fromJust)
import Data.String.Conversions (cs)
import Data.Swagger ( ToSchema (..), genericDeclareNamedSchema
                    , defaultSchemaOptions, schema
                    , description, example
                    )
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant

import Api.Common ( HasStatus (..), URL, StatusCode, Status (..)
                  , actOnStatus, concatTopic, concatURL
                  , csimRequest, csimRequestJSON
                  )
import Types (Self)

-- | The type specifying the interface's endpoints.
type MsueV1API
      -- List all registered UEs.
    = "api" :> "v1" :> "msue" :> Get '[JSON] [MsueUrlRef]

      -- Create a new UE with the given IMSI.
 :<|> "api" :> "v1" :> "msue" :> ReqBody '[JSON] MsueCtor
                              :> PostCreated '[JSON] MsueUrlRef

      -- Delete the referenced UE.
 :<|> "api" :> "v1" :> "msue" :> Capture "imsi" Text
                              :> DeleteNoContent '[JSON] NoContent

      -- List all the attributes for the references UE.
 :<|> "api" :> "v1" :> "msue" :> Capture "imsi" Text
                              :> Get '[JSON] [MsueAttributeDesc]

      -- Get the preferred EUtran cell attribute for the UE.
 :<|> "api" :> "v1" :> "msue" :> Capture "imsi" Text
                              :> "preferred-eutran-cell"
                              :> Get '[JSON] PciRef

      -- Set the preferred EUtran cell attribute for the UE.
 :<|> "api" :> "v1" :> "msue" :> Capture "imsi" Text
                              :> "preferred-eutran-cell"
                              :> ReqBody '[JSON] PciRef
                              :> PutNoContent '[JSON] NoContent

-- | JSON object to construct a new UE.
data MsueCtor = MsueCtor
    { imsi :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'MsueCtor'
instance ToSchema MsueCtor where
  declareNamedSchema proxy =
      genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped.schema.description ?~ "Msue constructor object"
      & mapped.schema.example ?~ toJSON (MsueCtor "123456")

-- | JSON object with one member, the pci of the preferred cell.
data PciRef = PciRef
    { pci :: !(Maybe Int)
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'PciRef'.
instance ToSchema PciRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object providing PCI"
            & mapped.schema.example ?~ toJSON (PciRef $ Just 3)

-- | JSON object with one member, the url to a UE resource.
data MsueUrlRef = MsueUrlRef
    { url :: !URL
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'MsueUrlRef'.
instance ToSchema MsueUrlRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object holding a resource URL"
            & mapped.schema.example ?~ toJSON (MsueUrlRef "/api/v1/msue/123456")

-- | Describe the attributes provided by a UE.
data MsueAttributeDesc = MsueAttributeDesc
    { url  :: !Text
    , desc :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'MsueAttributeDesc'.
instance ToSchema MsueAttributeDesc where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Msue attribute"
            & mapped.schema.example ?~
                toJSON MsueAttributeDesc
                    { url = "/api/v1/msue/123456/preferred-eutran-cell"
                    , desc = "Read or set the UE's preferred EUTRAN cell"
                    }

-- | List of UE Imsis, with status indicator.
data MsueImsiList = MsueImsiList
    { status :: !StatusCode
    , imsis  :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

instance HasStatus MsueImsiList where
    statusCode = status

-- | Preferred 'PciRef', with status indicator.
data MsuePciRef = MsuePciRef
    { status :: !StatusCode
    , pciRef :: !(Maybe PciRef)
    } deriving (Generic, Show, FromJSON, ToJSON)

instance HasStatus MsuePciRef where
    statusCode = status

-- | The service implementing the 'MsueV1API'.
msueV1Service :: Self -> Server MsueV1API
msueV1Service self
    = listUes self
 :<|> createUe self
 :<|> deleteUe self
 :<|> listAttributes self
 :<|> getPreferredEutranCell self
 :<|> setPreferredEutranCell self

-- | List all UEs. References to app.v1.msue.listPcos topic.
listUes :: Self -> Handler [MsueUrlRef]
listUes self =
    csimRequest self "app.v1.msue.listPcos" $ actOnStatus 200 handleReply
    where
        handleReply :: MsueImsiList -> [MsueUrlRef]
        handleReply MsueImsiList {..} =
            map (\imsi -> MsueUrlRef { url = concatURL [baseUrl, imsi] })
                (fromJust imsis)

-- | Create a new UE. References the app.v1.msue.createPco topic.
createUe :: Self -> MsueCtor -> Handler MsueUrlRef
createUe self msueCtor@MsueCtor {..} =
    csimRequestJSON self "app.v1.msue.createPco" msueCtor $
        actOnStatus 201 handleReply
    where
        handleReply :: Status -> MsueUrlRef
        handleReply _ = MsueUrlRef { url = concatURL [baseUrl, imsi ]}

-- | Delete an UE. References the app.v1.msue.deletePco.* topic.
deleteUe :: Self -> Text -> Handler NoContent
deleteUe self imsi = do
    let topic' = concatTopic ["app.v1.msue.deletePco", cs imsi]
    csimRequest self topic' $ actOnStatus 200 handleReply
    where
        handleReply :: Status -> NoContent
        handleReply _ = NoContent

-- | List a UE's attributes. The UE must exist. References the
-- app.v1.msue.*.exist topic.
listAttributes :: Self -> Text -> Handler [MsueAttributeDesc]
listAttributes self imsi = do
    let topic' = concatTopic ["app.v1.msue", cs imsi, "exist"]
    csimRequest self topic' $ actOnStatus 200 handleReply
    where
        handleReply :: Status -> [MsueAttributeDesc]
        handleReply _ =
            [ MsueAttributeDesc
                { url  = concatURL [baseUrl, imsi, "preferred-eutran-cell"]
                , desc = "Read or set the UE's preferred EUTRAN cell"
                }
            ]

-- | Get the preferred EUTRAN cell attribute for a UE. The UE must exist.
-- References the app.v1.msue.*.getPreferredEutranCell topic.
getPreferredEutranCell :: Self -> Text -> Handler PciRef
getPreferredEutranCell self imsi = do
    let topic' = concatTopic ["app.v1.msue", cs imsi, "getPreferredEutranCell"]
    csimRequest self topic' $ actOnStatus 200 handleReply
    where
        handleReply :: MsuePciRef -> PciRef
        handleReply MsuePciRef {..} = fromJust pciRef

-- | Set the preferred UETRAN cell for a UE. The UE must exist.
-- References the app.v1.msue.*.setPreferredEutranCell topic.
setPreferredEutranCell :: Self -> Text -> PciRef -> Handler NoContent
setPreferredEutranCell self imsi pciRef = do
    let topic' = concatTopic ["app.v1.msue", cs imsi, "setPreferredEutranCell"]
    csimRequestJSON self topic' pciRef $ actOnStatus 200 handleReply
    where
        handleReply :: Status -> NoContent
        handleReply _ = NoContent

baseUrl :: URL
baseUrl = "/api/v1/msue"
