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
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Nats
import Servant

import Api.Common (tmoRequest)
import Types (Self (..))

data NameRef = NameRef
    { name :: !Text
    } deriving (Generic, Show, FromJSON, ToJSON)

data UrlRef = UrlRef
    { url :: !Text
    } deriving (Generic, Show, FromJSON, ToJSON)

data CreateStatus = CreateStatus
    { status :: !Int
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | API for listing all registered Mmes.
type ListMmes = "api" :> "v1" :> "mme" :> Get '[JSON] [UrlRef]

-- | Request the app.v1.mme service to list all its pco names. If the
-- request is timing out, a 504 (Gateway Timeout) is returned. If the
-- requested payload not can be decoded, a 502 (Bad Gateway) is returned.
-- If everything is ok, 200 Ok is retured with a list of 'UrlRef'.
listMmes :: Self -> Handler [UrlRef]
listMmes Self {..} =
    tmoRequest tmo (request nats "app.v1.mme.listPcos" "") $ \msg ->
        maybe (throwError err502) (return . map toUrlRef) (jsonPayload msg)

type CreateMme = "api" :> "v1" :> "mme"
                       :> ReqBody '[JSON] NameRef
                       :> PostCreated '[JSON] UrlRef

-- | Request the app.v1.mme.createPco service to create a new pco with
-- the given name.
createMme :: Self -> NameRef -> Handler UrlRef
createMme Self {..} NameRef {..} = do
    let topic' = "app.v1.mme.createPco." `mappend` (cs name)
    tmoRequest tmo (request nats topic' "") $ \msg ->
        maybe (throwError err502) handleStatus (jsonPayload msg)
    where
        handleStatus :: CreateStatus -> Handler UrlRef
        handleStatus CreateStatus {..} =
            case status of
                201 -> return $ toUrlRef name
                409 -> throwError err409
                _   -> throwError err502

type DeleteMme = "api" :> "v1" :> "mme" :> Capture "name" Text
                       :> DeleteNoContent '[JSON] NoContent

deleteMme :: Self -> Text -> Handler NoContent
deleteMme = undefined

type GetIpConfig = "api" :> "v1" :> "mme" :> Capture "name" Text
                         :> Get '[JSON] [Text]

getIpConfig :: Self -> Text -> Handler [Text]
getIpConfig = undefined

toUrlRef :: Text -> UrlRef
toUrlRef mmeName =
    UrlRef { url = "/api/v1/mme/" `mappend` mmeName }
