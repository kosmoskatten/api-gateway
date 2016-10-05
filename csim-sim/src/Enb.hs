{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Main
    ( PlmnId (..)
    , main
    ) where

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Nats

import Common ( Storage, emptyStorage, maybeInsert, maybeDelete
              , listKeys, stayAlive, splitTopic, ifReply
              )
import Options (Options (..), getOptions)

data PlmnId = PlmnId
    { mcc       :: !Int
    , mnc       :: !Int
    , mncLength :: !Int
    } deriving (Generic, Show, FromJSON, ToJSON)

data CreatePco = CreatePco
    { name   :: !Text
    , plmnId :: !PlmnId
    } deriving (Generic, Show, FromJSON, ToJSON)

data Status = Status
    { status :: !Int
    } deriving (Generic, Show, FromJSON, ToJSON)

data EnbNameList = EnbNameList
    { status :: !Int
    , names  :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

type EnbMap = Storage PlmnId

data Self = Self
    { enbMap :: !EnbMap
    }

main :: IO ()
main = do
    options <- getOptions
    self    <- Self <$> atomically emptyStorage

    -- Connect to NATS.
    withNats defaultSettings [natsUri options] $ \nats -> do

        -- Subscribe to the createPco topic.
        void $ subscribeAsync nats "app.v1.enb.createPco"
                              Nothing (createPco nats self)

        -- Subscribe to the deletePco topic.
        void $ subscribeAsync nats "app.v1.enb.deletePco.*"
                              Nothing (deletePco nats self)

        -- Subscribe to the listPcos topic.
        void $ subscribeAsync nats "app.v1.enb.listPcos"
                              Nothing (listPcos nats self)

        -- Make the main thread staying alive.
        stayAlive

-- | Create a new eNodeB pco.
createPco :: Nats -> Self -> Msg -> IO ()
createPco nats self msg =
    ifReply msg $ \reply -> do
        maybe (publishJson nats reply Nothing Status { status = 400 })
              (\json -> publishJson nats reply Nothing =<< createPco' json)
              (jsonPayload msg)
    where
        createPco' :: CreatePco -> IO Status
        createPco' CreatePco {..} = do
            inserted <- atomically $ maybeInsert (enbMap self) name plmnId
            if inserted
                then return Status { status = 201 }
                else return Status { status = 409 }

deletePco :: Nats -> Self -> Msg -> IO ()
deletePco nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, _, name] = splitTopic $ topic msg
        publishJson nats reply Nothing =<< deletePco' (cs name)
    where
        deletePco' :: Text -> IO Status
        deletePco' name = do
            deleted <- atomically $ maybeDelete (enbMap self) name
            if deleted
                then return Status { status = 200 }
                else return Status { status = 404 }

listPcos :: Nats -> Self -> Msg -> IO ()
listPcos nats self msg =
    ifReply msg $ \reply -> publishJson nats reply Nothing =<< listPcos'
    where
        listPcos' :: IO EnbNameList
        listPcos' = do
            ns <- atomically $ listKeys (enbMap self)
            return EnbNameList { status = 200, names = Just ns }
