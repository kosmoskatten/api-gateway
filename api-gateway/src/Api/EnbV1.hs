{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

-- | Module implementing version 1 of the eNodeB REST API.
module Api.EnbV1
    ( EnbV1API
    , enbV1Service
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
type EnbV1API
      -- List all registered eNodeBs.
    = "api" :> "v1" :> "enb" :> Get '[JSON] [EnbUrlRef]

      -- Create a new eNodeB.
 :<|> "api" :> "v1" :> "enb" :> ReqBody '[JSON] EnbCtor
                             :> PostCreated '[JSON] EnbUrlRef

      -- Delete an eNodeB.
 :<|> "api" :> "v1" :> "enb" :> Capture "name" Text
                             :> DeleteNoContent '[JSON] NoContent

data EnbCtor = EnbCtor
    { name   :: !Text
    , enbId  :: !Int
    , plmnId :: !PlmnId
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'EnbCtor'.
instance ToSchema EnbCtor where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "eNodeB constructor object"
            & mapped.schema.example ?~
                toJSON EnbCtor { name   = "enb1"
                               , enbId  = 1001
                               , plmnId = PlmnId { mcc       = 234
                                                 , mnc       = 89
                                                 , mncLength = 2
                                                 }
                                }


data PlmnId = PlmnId
    { mcc       :: !Int
    , mnc       :: !Int
    , mncLength :: !Int
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'PlmnId'.
instance ToSchema PlmnId where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object describing a plmn id"
            & mapped.schema.example ?~
                toJSON PlmnId { mcc       = 234
                              , mnc       = 89
                              , mncLength = 2
                              }

-- | JSON object with one member, the url to an eNodeB resource.
data EnbUrlRef = EnbUrlRef
    { url :: !Text
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'EnbUrlRef'.
instance ToSchema EnbUrlRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object holding a resource URL"
            & mapped.schema.example ?~ toJSON (EnbUrlRef "/api/v1/enb/enb1")

-- | The service implementing 'EnbV1API'.
enbV1Service :: Self -> Server EnbV1API
enbV1Service = undefined
