module Common
    ( Storage
    , emptyStorage
    , maybeInsert
    , maybeDelete
    , listKeys
    , isMember
    , getItem
    , setItem
    , stayAlive
    , splitTopic
    , ifReply
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap

import Network.Nats (Msg, Topic, replyTo)

-- | Concurrent storage type. Assumes 'Text' beeing key.
type Storage a = TVar (HashMap Text a)

-- | Create a new, empty, storage.
emptyStorage :: STM (Storage a)
emptyStorage = newTVar HashMap.empty

-- | Insert the provided item to the storage. If the key already is part
-- of the storage the insert will fail and 'False' is returned. Otherwise
-- the value of 'True' is returned.
maybeInsert :: Storage a -> Text -> a -> STM Bool
maybeInsert storage key item = do
    hm <- readTVar storage
    case HashMap.member key hm of
        False -> (writeTVar storage $ HashMap.insert key item hm) >> return True
        True  -> return False

-- | Delete an item from the storage. If the key not is part of the storage
-- the delete will fail and 'False' is returned. Otherwise 'True' is returned.
maybeDelete :: Storage a -> Text -> STM Bool
maybeDelete storage key = do
    hm <- readTVar storage
    case HashMap.member key hm of
        True  -> (writeTVar storage $ HashMap.delete key hm) >> return True
        False -> return False

-- | List all the keys from the storage.
listKeys :: Storage a -> STM [Text]
listKeys storage = HashMap.keys <$> readTVar storage

-- | Check if the key is member of the storage.
isMember :: Storage a -> Text -> STM Bool
isMember storage key = HashMap.member key <$> readTVar storage

-- | Get the item related to the key.
getItem :: Storage a -> Text -> STM (Maybe a)
getItem storage key = HashMap.lookup key <$> readTVar storage

setItem :: Storage a -> Text -> a -> STM Bool
setItem storage key item = do
    hm <- readTVar storage
    case HashMap.member key hm of
        True  -> (writeTVar storage $ HashMap.insert key item hm) >> return True
        False -> return False

stayAlive :: IO ()
stayAlive =
    forever $ threadDelay 5000000

splitTopic :: Topic -> [Topic]
splitTopic = BS.split '.'

ifReply :: Msg -> (Topic -> IO ()) -> IO ()
ifReply msg action = maybe (return ()) action (replyTo msg)
