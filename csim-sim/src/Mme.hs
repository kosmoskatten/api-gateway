{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main
    ( main
    ) where

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Nats

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Common (stayAlive, splitTopic)
import Options (Options (..), getOptions)

data Status = Status
    { status :: !Int
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
        void $ subscribeAsync nats "app.v1.mme.createPco.*"
                              Nothing (createPco nats self)

        -- Subscribe to tje deletePco topic.
        void $ subscribeAsync nats "app.v1.mme.deletePco.*"
                              Nothing (deletePco nats self)

        -- Subscribe to the listPcos topic.
        void $ subscribeAsync nats "app.v1.mme.listPcos"
                              Nothing (listPcos nats self)

        -- Subscribe to the getIpConfig topic.
        void $ subscribeAsync nats "app.v1.mme.*.getIpConfig"
                              Nothing (getIpConfig nats self)

        -- Make the main thread staying alive.
        stayAlive

-- | Create a new MME pco. The name for the MME to be created is the last
-- part of the topic.
createPco :: Nats -> Self -> Msg -> IO ()
createPco nats self msg =
    case replyTo msg of
        Just reply -> do
            let [_, _, _, _, name] = splitTopic $ topic msg
                name'              = cs name
            publishJson nats reply Nothing =<< createPco' name'

        Nothing    -> return ()
    where
        createPco' :: Text -> IO Status
        createPco' name = do
            inserted <- atomically $ maybeInsertMme name
            if inserted
                then return $ Status { status = 201 }
                else return $ Status { status = 409 }

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
    case replyTo msg of
        Just reply -> do
            let [_, _, _, _, name] = splitTopic $ topic msg
                name'              = cs name
            publishJson nats reply Nothing =<< deletePco' name'

        Nothing    -> return ()
    where
        deletePco' :: Text -> IO Status
        deletePco' name = do
            deleted <- atomically $ maybeDeleteMme name
            if deleted
                then return $ Status { status = 200 }
                else return $ Status { status = 404 }

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
    case replyTo msg of
        Just reply ->
            publishJson nats reply Nothing =<< listPcos'

        Nothing    -> return ()
    where
        listPcos' :: IO [Text]
        listPcos' = HashMap.keys <$> readTVarIO (mmeMap self)

-- | Get the IP config for the given MME.
getIpConfig :: Nats -> Self -> Msg -> IO ()
getIpConfig nats self msg =
    case replyTo msg of
        Just reply -> do
            let [_, _, _, name, _] = splitTopic $ topic msg
                name'              = cs name
            publishJson nats reply Nothing =<< getIpConfig' name'

        Nothing -> return ()
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
