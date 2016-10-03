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
type EnbV1API
      -- List all registered eNodeBs.
    = "api" :> "v1" :> "enb" :> Get '[JSON] [EnbUrlRef]

      -- Create a new eNodeB.
 :<|> "api" :> "v1" :> "enb" :> ReqBody '[JSON] EnbCtor
                             :> PostCreated '[JSON] EnbUrlRef

      -- Delete an eNodeB.
 :<|> "api" :> "v1" :> "enb" :> Capture "name" Text
                             :> DeleteNoContent '[JSON] NoContent

-- | JSON object to construct a new eNodeB.
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
    { url :: !URL
    } deriving (Generic, Show, Typeable, FromJSON, ToJSON)

-- | Swagger schema for 'EnbUrlRef'.
instance ToSchema EnbUrlRef where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.schema.description ?~ "Object holding a resource URL"
            & mapped.schema.example ?~ toJSON (EnbUrlRef "/api/v1/enb/enb1")

-- | List of eNodeB names, with status indicator.
data EnbNameList = EnbNameList
    { status :: !StatusCode
    , names  :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

instance HasStatus EnbNameList where
    statusCode = status

-- | The service implementing 'EnbV1API'.
enbV1Service :: Self -> Server EnbV1API
enbV1Service self
    = listEnbs self
 :<|> createEnb self
 :<|> deleteEnb self

-- | List all eNodeBs. References the app.v1.enb.listPcos topic.
listEnbs :: Self -> Handler [EnbUrlRef]
listEnbs self =
    csimRequest self "app.v1.enb.listPcos" $ actOnStatus 200 handleReply
    where
        handleReply :: EnbNameList -> [EnbUrlRef]
        handleReply EnbNameList {..} =
            map (\name -> EnbUrlRef { url = concatURL [baseUrl, name] })
                (fromJust names)

-- | Create a new eNodeB. References the app.v1.enb.createPco topic.
createEnb :: Self -> EnbCtor -> Handler EnbUrlRef
createEnb self enbCtor@EnbCtor {..} =
    csimRequestJSON self "app.v1.enb.createPco" enbCtor $
        actOnStatus 201 handleReply
    where
        handleReply :: Status -> EnbUrlRef
        handleReply _ = EnbUrlRef { url = concatURL [baseUrl, name] }

-- | Delete an eNodeB. References the app.v1.enb.deletePco.* topic.
deleteEnb :: Self -> Text -> Handler NoContent
deleteEnb self name = do
    let topic' = concatTopic ["app.v1.enb.deletePco.*", cs name]
    csimRequest self topic' $ actOnStatus 200 handleReply
    where
        handleReply :: Status -> NoContent
        handleReply _ = NoContent

baseUrl :: URL
baseUrl = "/api/v1/enb"
