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
import Data.Swagger ( ToSchema (..), genericDeclareNamedSchema
                    , defaultSchemaOptions, schema
                    , description, example
                    )
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Nats
import Servant

import Api.Common ( URL, Status (..), concatTopic, concatURL
                  , tmoRequest, translateErrCode
                  )
import Types (Self (..))

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

      -- Get the preferred EUtran cell attribute for the UE.
 :<|> "api" :> "v1" :> "msue" :> Capture "imsi" Text
                              :> "preferred-eutran-cell"
                              :> Get '[JSON] PciRef

      -- Set the preferred EUtran cell attribute for the UE.
 :<|> "api" :> "v1" :> "msue" :> Capture "imsi" Text
                              :> "preferred-eutran-cell"
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

-- | List of UE Imsis, with status indicator.
data UeImsiList = UeImsiList
    { status :: !Int
    , imsis  :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | The service implementing the 'MsueV1API'.
msueV1Service :: Self -> Server MsueV1API
msueV1Service self
    = listUes self
 :<|> createUe self
 :<|> undefined

-- | List all UEs. References to app.v1.msue.listPcos topic.
listUes :: Self -> Handler [MsueUrlRef]
listUes Self {..} =
    tmoRequest tmo (request nats "app.v1.msue.listPcos" "") $ \msg ->
        maybe (throwError err502) handleReply (jsonPayload msg)
    where
        handleReply :: UeImsiList -> Handler [MsueUrlRef]
        handleReply UeImsiList {..} =
            case status of
                -- 200 is the expected positive status.
                200 -> return $ map
                    (\i -> MsueUrlRef { url = concatURL [baseUrl, i] })
                    (fromJust imsis)

                -- Catch all the rest as error codes.
                _   -> translateErrCode status

-- | Create a new UE. References the app.v1.msue.createPco topic.
createUe :: Self -> MsueCtor -> Handler MsueUrlRef
createUe Self {..} msueCtor@MsueCtor {..} =
    tmoRequest tmo (requestJson nats "app.v1.msue.createPco" msueCtor) $ \msg ->
        maybe (throwError err502) handleReply (jsonPayload msg)
    where
        handleReply :: Status -> Handler MsueUrlRef
        handleReply Status {..} =
            case status of
                -- 201 is the expected positive status.
                201 -> return MsueUrlRef { url = concatURL [baseUrl, imsi ]}

                -- Catch all the rest as error codes.
                _   -> translateErrCode status

baseUrl :: URL
baseUrl = "/api/v1/msue"
