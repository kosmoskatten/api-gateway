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
import Data.Swagger ( ToSchema (..), genericDeclareNamedSchema
                    , defaultSchemaOptions, schema
                    , description, example
                    )
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant

import Types (Self (..))

-- | The type specifying the interface's endpoints.
type MsueV1API
      -- List all registered UEs.
    = "api" :> "v1" :> "msue" :> Get '[JSON] [UrlRef]

      -- Create a new UE with the given IMSI.
 :<|> "api" :> "v1" :> "msue" :> ReqBody '[JSON] ImsiRef
                              :> PostCreated '[JSON] UrlRef

      -- Delete the referenced UE.
 :<|> "api" :> "v1" :> "msue" :> Capture "imsi" Text
                              :> DeleteNoContent '[JSON] NoContent

data PciRef = PciRef
    { pci :: !Int
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

instance ToSchema PciRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object providing PCI"
            & mapped.schema.example ?~ toJSON (PciRef 3)

-- | JSON object with one member, the imsi of the UE to be created.
data ImsiRef = ImsiRef
    { imsi :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'ImsiRef'
instance ToSchema ImsiRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object providing UE IMSI"
            & mapped.schema.example ?~ toJSON (ImsiRef "123456")

-- | JSON object with one member, the url to a UE resource.
data UrlRef = UrlRef
    { url :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'UrlRef'.
instance ToSchema UrlRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object holding a resource URL"
            & mapped.schema.example ?~ toJSON (UrlRef "/api/v1/msue/123456")

-- | The service implementing the 'MsueV1API'.
msueV1Service :: Self -> Server MsueV1API
msueV1Service = undefined
