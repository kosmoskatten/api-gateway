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

import Api.Common (URL)
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
    { pci :: !Int
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'PciRef'.
instance ToSchema PciRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object providing PCI"
            & mapped.schema.example ?~ toJSON (PciRef 3)

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

-- | The service implementing the 'MsueV1API'.
msueV1Service :: Self -> Server MsueV1API
msueV1Service = undefined
