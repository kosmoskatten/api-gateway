{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Main
    ( main
    ) where

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Nats

import qualified Data.Text as Text

import Common ( Storage, emptyStorage, maybeInsert, maybeDelete
              , isMember, listKeys, getItem
              , stayAlive, splitTopic, ifReply
              )
import Options (Options (..), getOptions)

data CreatePco = CreatePco
    { name :: !Text
    } deriving (Generic, Show, FromJSON, ToJSON)

data Status = Status
    { status :: !Int
    } deriving (Generic, Show, FromJSON, ToJSON)

data MmeNameList = MmeNameList
    { status :: !Int
    , names  :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

data IpConfig = IpConfig
    { status :: !Int
    , config :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Map from a mme name to a list of IP addresses (all represented as 'Text').
type MmeMap = Storage [Text]

data Self = Self
    { mmeMap :: !MmeMap
    , nextIp :: !(TVar Int)
    }

main :: IO ()
main = do
    options <- getOptions
    self    <- Self <$> atomically emptyStorage
                    <*> newTVarIO 1

    -- Connect to NATS.
    withNats defaultSettings [natsUri options] $ \nats -> do

        -- Subscribe to the createPro topic.
        void $ subscribeAsync nats "app.v1.mme.createPco"
                              Nothing (createPco nats self)

        -- Subscribe to the deletePco topic.
        void $ subscribeAsync nats "app.v1.mme.deletePco.*"
                              Nothing (deletePco nats self)

        -- Subscribe to the listPcos topic.
        void $ subscribeAsync nats "app.v1.mme.listPcos"
                              Nothing (listPcos nats self)

        -- Subscribe to the exist topic.
        void $ subscribeAsync nats "app.v1.mme.*.exist"
                              Nothing (exist nats self)

        -- Subscribe to the getIpConfig topic.
        void $ subscribeAsync nats "app.v1.mme.*.getIpConfig"
                              Nothing (getIpConfig nats self)

        -- Make the main thread staying alive.
        stayAlive

-- | Create a new MME pco.
createPco :: Nats -> Self -> Msg -> IO ()
createPco nats self msg =
    ifReply msg $ \reply -> do
        maybe (publishJson nats reply Nothing $ Status 400)
              (\json -> publishJson nats reply Nothing =<< createPco' json)
              (jsonPayload msg)
    where
        createPco' :: CreatePco -> IO Status
        createPco' ctor = do
            inserted <- atomically $ maybeInsertMme ctor
            if inserted
                then return Status { status = 201 }
                else return Status { status = 409 }

        maybeInsertMme :: CreatePco -> STM Bool
        maybeInsertMme CreatePco {..} = do
            isPartOf <- isMember (mmeMap self) name
            if isPartOf
                then return False
                else do
                    ip <- readTVar (nextIp self)
                    modifyTVar (nextIp self) (+1)
                    maybeInsert (mmeMap self) name [ipAddress ip]

deletePco :: Nats -> Self -> Msg -> IO ()
deletePco nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, _, name] = splitTopic $ topic msg
        publishJson nats reply Nothing =<< deletePco' (cs name)
    where
        deletePco' :: Text -> IO Status
        deletePco' name = do
            deleted <- atomically $ maybeDelete (mmeMap self) name
            if deleted
                then return Status { status = 200 }
                else return Status { status = 404 }

-- | List all MME pcos.
listPcos :: Nats -> Self -> Msg -> IO ()
listPcos nats self msg =
    ifReply msg $ \reply -> publishJson nats reply Nothing =<< listPcos'
    where
        listPcos' :: IO MmeNameList
        listPcos' = do
            ns <- atomically $ listKeys (mmeMap self)
            return MmeNameList { status = 200, names = Just ns }

-- | Check existance of the MME.
exist :: Nats -> Self -> Msg -> IO ()
exist nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, name, _] = splitTopic $ topic msg
        found <- atomically $ isMember (mmeMap self) (cs name)
        if found
            then publishJson nats reply Nothing Status { status = 200 }
            else publishJson nats reply Nothing Status { status = 404 }

-- | Get the IP config for the given MME.
getIpConfig :: Nats -> Self -> Msg -> IO ()
getIpConfig nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, name, _] = splitTopic $ topic msg
        publishJson nats reply Nothing =<< getIpConfig' (cs name)
    where
        getIpConfig' :: Text -> IO IpConfig
        getIpConfig' name = do
            maybeConfig <- atomically $ getItem (mmeMap self) name
            case maybeConfig of
                Just c  -> return $ IpConfig { status = 200, config = Just c }
                Nothing -> return $ IpConfig { status = 404, config = Nothing }

-- | Generate a new pseudo IP address.
ipAddress :: Int -> Text
ipAddress n = "192.168.1." `Text.append` (Text.pack $ show n)
