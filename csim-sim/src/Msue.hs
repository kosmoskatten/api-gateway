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

import Common ( Storage, emptyStorage, maybeInsert, maybeDelete
              , isMember, listKeys, getItem, setItem
              , stayAlive, splitTopic, ifReply
              )
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
type UeMap = Storage (Maybe Int)

data Self = Self
    { ueMap :: !UeMap
    }

main :: IO ()
main = do
    options <- getOptions
    self    <- Self <$> atomically emptyStorage

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
            is <- atomically $ listKeys (ueMap self)
            return UeImsiList { status = 200, imsis = Just is }

-- | Create a new UE pco.
createPco :: Nats -> Self -> Msg -> IO ()
createPco nats self msg =
    ifReply msg $ \reply ->
        maybe (publishJson nats reply Nothing Status { status = 400 })
              (\json -> publishJson nats reply Nothing =<< createPco' json)
              (jsonPayload msg)
    where
        createPco' :: CreatePco -> IO Status
        createPco' CreatePco {..} = do
            inserted <- atomically $ maybeInsert (ueMap self) imsi Nothing
            if inserted
                then return Status { status = 201 }
                else return Status { status = 409 }

-- Delete the named UE pco.
deletePco :: Nats -> Self -> Msg -> IO ()
deletePco nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, _, imsi] = splitTopic $ topic msg
        publishJson nats reply Nothing =<< deletePco' (cs imsi)
    where
        deletePco' :: Text -> IO Status
        deletePco' imsi = do
            deleted <- atomically $ maybeDelete (ueMap self) imsi
            if deleted
                then return $ Status 200
                else return $ Status 404

exist :: Nats -> Self -> Msg -> IO ()
exist nats self msg =
    ifReply msg $ \reply -> do
        let [_, _, _, imsi, _] = splitTopic (topic msg)
        found <- atomically $ isMember (ueMap self) (cs imsi)
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
            maybeCell <- atomically $ getItem (ueMap self) imsi
            case maybeCell of
                Just cell -> return $ UePciRef { status = 200, pci = cell }
                Nothing   -> return $ UePciRef { status = 404, pci = Nothing }

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
        setCell imsi PciRef {..} = do
            isSet <- atomically $ setItem (ueMap self) imsi pci
            if isSet
                then return $ Status 200
                else return $ Status 404
