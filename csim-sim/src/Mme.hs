{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main
    ( main
    ) where

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Nats

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Common (stayAlive, splitTopic, ifReply)
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
type MmeMap = HashMap Text [Text]

data Self = Self
    { mmeMap :: !(TVar MmeMap)
    , nextIp :: !(TVar Int)
    }

main :: IO ()
main = do
    options <- getOptions
    self    <- Self <$> newTVarIO HashMap.empty
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
            inserted <- atomically $ maybeInsertMme (name ctor)
            if inserted
                then return Status { status = 201 }
                else return Status { status = 409 }

        maybeInsertMme :: Text -> STM Bool
        maybeInsertMme name = do
            mmes <- readTVar $ mmeMap self
            case HashMap.lookup name mmes of
                Just _  -> return False
                Nothing -> do
                    ip <- readTVar (nextIp self)
                    modifyTVar (nextIp self) (+1)
                    writeTVar (mmeMap self) $
                        HashMap.insert name [ipAddress ip] mmes
                    return True

deletePco :: Nats -> Self -> Msg -> IO ()
deletePco nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, _, name] = splitTopic $ topic msg
        publishJson nats reply Nothing =<< deletePco' (cs name)
    where
        deletePco' :: Text -> IO Status
        deletePco' name = do
            deleted <- atomically $ maybeDeleteMme name
            if deleted
                then return Status { status = 200 }
                else return Status { status = 404 }

        maybeDeleteMme :: Text -> STM Bool
        maybeDeleteMme name = do
            mmes <- readTVar $ mmeMap self
            case HashMap.lookup name mmes of
                Just _  -> do
                    modifyTVar (mmeMap self) $ HashMap.delete name
                    return True
                Nothing -> return False

-- | List all MME pcos.
listPcos :: Nats -> Self -> Msg -> IO ()
listPcos nats self msg =
    ifReply msg $ \reply -> publishJson nats reply Nothing =<< listPcos'
    where
        listPcos' :: IO MmeNameList
        listPcos' = do
            ns <- HashMap.keys <$> readTVarIO (mmeMap self)
            return MmeNameList { status = 200, names = Just ns }

-- | Check existance of the MME.
exist :: Nats -> Self -> Msg -> IO ()
exist nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, name, _] = splitTopic $ topic msg
        found <- HashMap.member (cs name) <$> readTVarIO (mmeMap self)
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
            maybeConfig <- HashMap.lookup name <$> readTVarIO (mmeMap self)
            case maybeConfig of
                Just c  -> return $ IpConfig { status = 200, config = Just c }
                Nothing -> return $ IpConfig { status = 404, config = Nothing }

-- | Generate a new pseudo IP address.
ipAddress :: Int -> Text
ipAddress n = "192.168.1." `Text.append` (Text.pack $ show n)
