{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Api.MmeV1
    ( NameRef (..)
    , UrlRef (..)

    , ListMmes
    , listMmes

    , CreateMme
    , createMme

    , DeleteMme
    , deleteMme

    , GetIpConfig
    , getIpConfig
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

import Types (Self (..))

data NameRef = NameRef
    { name :: !Text
    } deriving (Generic, Show, FromJSON, ToJSON)

data UrlRef = UrlRef
    { url :: !Text
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | API for listing all registered Mmes.
type ListMmes = "api" :> "v1" :> "mme" :> Get '[JSON] [UrlRef]

listMmes :: Self -> Handler [UrlRef]
listMmes _ = undefined

type CreateMme = "api" :> "v1" :> "mme"
                       :> ReqBody '[JSON] NameRef
                       :> PostCreated '[JSON] UrlRef

createMme :: Self -> NameRef -> Handler UrlRef
createMme = undefined

type DeleteMme = "api" :> "v1" :> "mme" :> Capture "name" Text
                       :> DeleteNoContent '[JSON] NoContent

deleteMme :: Self -> Text -> Handler NoContent
deleteMme = undefined

type GetIpConfig = "api" :> "v1" :> "mme" :> Capture "name" Text
                         :> Get '[JSON] [Text]

getIpConfig :: Self -> Text -> Handler [Text]
getIpConfig = undefined
