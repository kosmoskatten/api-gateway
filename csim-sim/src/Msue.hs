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

        -- Subscribe to the createPco topic.
        void $ subscribeAsync nats "app.v1.msue.createPco"
                              Nothing (createPco nats self)

        -- Subscribe to the listPcos topic.
        void $ subscribeAsync nats "app.v1.msue.listPcos"
                              Nothing (listPcos nats self)

        -- Make the main thread staying alive.
        stayAlive

-- | Create a new UE pco.
createPco :: Nats -> Self -> Msg -> IO ()
createPco nats self msg =
    ifReply msg $ \reply ->
        maybe (publishJson nats reply Nothing $ Status 400)
              (\json -> publishJson nats reply Nothing =<< createPco' json)
              (jsonPayload msg)
    where
        createPco' :: CreatePco -> IO Status
        createPco' msg = do
            inserted <- atomically $ maybeInsertUe (imsi msg)
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

-- | List all Msue pcos.
listPcos :: Nats -> Self -> Msg -> IO ()
listPcos nats self msg =
    ifReply msg $ \reply -> publishJson nats reply Nothing =<< listPcos'
    where
        listPcos' :: IO UeImsiList
        listPcos' = do
            is <- HashMap.keys <$> readTVarIO (ueMap self)
            return UeImsiList { status = 200, imsis = Just is }
