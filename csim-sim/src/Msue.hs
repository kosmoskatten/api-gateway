{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Main
( Status (..)
, UePciRef (..)
, main
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

import Common (stayAlive, splitTopic, ifReply)
import Options (Options (..), getOptions)

data CreatePco = CreatePco
    { imsi :: !Text
    } deriving (Generic, Show, FromJSON, ToJSON)

data Status = Status
    { status :: !Int
    } deriving (Generic, Show, FromJSON, ToJSON)

data UeImsiList = UeImsiList
    { status :: !Int
    , imsis  :: !(Maybe [Text])
    } deriving (Generic, Show, FromJSON, ToJSON)

data PciRef = PciRef
    { pci :: !(Maybe Int)
    } deriving (Generic, Show, FromJSON, ToJSON)

data UePciRef = UePciRef
    { status :: !Int
    , pci    :: !(Maybe Int)
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Map from an Ue Imsi to an optional pci number.
type UeMap = HashMap Text (Maybe Int)

data Self = Self
    { ueMap :: !(TVar UeMap)
    }

main :: IO ()
main = do
    options <- getOptions
    self    <- Self <$> newTVarIO HashMap.empty

    -- Connect to NATS.
    withNats defaultSettings [natsUri options] $ \nats -> do

        -- Subscribe to the listPcos topic.
        void $ subscribeAsync nats "app.v1.msue.listPcos"
                              Nothing (listPcos nats self)

        -- Subscribe to the createPco topic.
        void $ subscribeAsync nats "app.v1.msue.createPco"
                              Nothing (createPco nats self)

        -- Subscribe to the deletePco topic.
        void $ subscribeAsync nats "app.v1.msue.deletePco.*"
                              Nothing (deletePco nats self)

        -- Subscribe to the exist topic.
        void $ subscribeAsync nats "app.v1.msue.*.exist"
                              Nothing (exist nats self)

        -- Subscribe to the getPreferredEutranCell topic.
        void $ subscribeAsync nats "app.v1.msue.*.getPreferredEutranCell"
                              Nothing (getPreferredEutranCell nats self)

        -- Subscribe to the setPreferredEutranCell topic.
        void $ subscribeAsync nats "app.v1.msue.*.setPreferredEutranCell"
                              Nothing (setPreferredEutranCell nats self)

        -- Make the main thread staying alive.
        stayAlive

-- | List all Msue pcos.
listPcos :: Nats -> Self -> Msg -> IO ()
listPcos nats self msg =
    ifReply msg $ \reply -> publishJson nats reply Nothing =<< listPcos'
    where
        listPcos' :: IO UeImsiList
        listPcos' = do
            is <- HashMap.keys <$> readTVarIO (ueMap self)
            return UeImsiList { status = 200, imsis = Just is }

-- | Create a new UE pco.
createPco :: Nats -> Self -> Msg -> IO ()
createPco nats self msg =
    ifReply msg $ \reply ->
        maybe (publishJson nats reply Nothing $ Status 400)
              (\json -> publishJson nats reply Nothing =<< createPco' json)
              (jsonPayload msg)
    where
        createPco' :: CreatePco -> IO Status
        createPco' ctor = do
            inserted <- atomically $ maybeInsertUe (imsi ctor)
            if inserted
                then return $ Status 201
                else return $ Status 409

        maybeInsertUe :: Text -> STM Bool
        maybeInsertUe imsi = do
            ues <- readTVar $ ueMap self
            case HashMap.lookup imsi ues of
                Just _  -> return False
                Nothing -> do
                    writeTVar (ueMap self) $
                        HashMap.insert imsi Nothing ues
                    return True

-- Delete the named UE pco.
deletePco :: Nats -> Self -> Msg -> IO ()
deletePco nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, _, imsi] = splitTopic $ topic msg
        publishJson nats reply Nothing =<< deletePco' (cs imsi)
    where
        deletePco' :: Text -> IO Status
        deletePco' imsi = do
            deleted <- atomically $ maybeDeleteUe imsi
            if deleted
                then return $ Status 200
                else return $ Status 404

        maybeDeleteUe :: Text -> STM Bool
        maybeDeleteUe imsi = do
            ues <- readTVar (ueMap self)
            if HashMap.member imsi ues
                then do writeTVar (ueMap self) (HashMap.delete imsi ues)
                        return True
                else return False

exist :: Nats -> Self -> Msg -> IO ()
exist nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, imsi, _] = splitTopic (topic msg)
        found <- HashMap.member (cs imsi) <$> readTVarIO (ueMap self)
        if found
            then publishJson nats reply Nothing $ Status 200
            else publishJson nats reply Nothing $ Status 404

getPreferredEutranCell :: Nats -> Self -> Msg -> IO ()
getPreferredEutranCell nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, imsi, _] = splitTopic (topic msg)
        publishJson nats reply Nothing =<< getCell (cs imsi)
    where
        getCell :: Text -> IO UePciRef
        getCell imsi = do
            ues <- readTVarIO $ ueMap self
            case HashMap.lookup imsi ues of
                Just cell -> return $ UePciRef 200 cell
                Nothing   -> return $ UePciRef 404 Nothing

setPreferredEutranCell :: Nats -> Self -> Msg -> IO ()
setPreferredEutranCell nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, imsi, _] = splitTopic (topic msg)
        maybe (publishJson nats reply Nothing $ Status 400)
              (\cell -> publishJson nats reply Nothing
                =<< setCell (cs imsi) cell)
              (jsonPayload msg)
    where
        setCell :: Text -> PciRef -> IO Status
        setCell imsi pciRef = do
            isSet <- atomically $ maybeSetCell imsi pciRef
            if isSet
                then return $ Status 200
                else return $ Status 404

        maybeSetCell :: Text -> PciRef -> STM Bool
        maybeSetCell imsi PciRef {..} = do
            ues <- readTVar (ueMap self)
            if HashMap.member imsi ues
                then do writeTVar (ueMap self)
                                  (HashMap.insert imsi pci ues)
                        return True
                else return False
