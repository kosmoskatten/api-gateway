{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Data.Text (Text)
import Network.Nats

main :: IO ()
main =
    withNats defaultSettings ["nats://localhost:4222"] $ \nats -> do
        void $ subscribeAsync nats "app.v1.mme.listPcos"
                              Nothing (listPcos nats)
        keepAlive

keepAlive :: IO ()
keepAlive =
    forever $ threadDelay 5000000

listPcos :: Nats -> Msg -> IO ()
listPcos nats msg =
    case replyTo msg of
        Just reply -> publishJson nats reply Nothing ["mme1" :: Text]
        Nothing    -> return ()
