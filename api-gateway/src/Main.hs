{-# LANGUAGE DuplicateRecordFields #-}
module Main
    ( main
    ) where

import Control.Monad (when)
import Data.Default.Class (Default (def))
import Network.Nats (withNats, defaultSettings)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
import Options.Applicative
import System.Exit (exitSuccess)
import System.Log.FastLogger (defaultBufSize, newFileLoggerSet)
import Text.Printf (printf)

import qualified Data.ByteString.Lazy.Char8 as LBS

import Api (app, prettySwagger)
import Types (Self (..), TmoSec (..))

-- | Command line options.
data Options = Options
    { natsUri        :: !String
      -- ^ The URI for the NATS server to connect to.

    , apiPort        :: !Int
      -- ^ The network port on which the API Gateway shall listen.

    , staticDir      :: !FilePath
      -- ^ The root directory for static files.

    , timeout        :: !Int
      -- ^ A timeout value in seconds how long the API Gateway shall wait
      -- until a component answer a request.

    , logDest        :: !(Maybe FilePath)
      -- ^ A specified log destination (otherwise stdout).

    , logType        :: !LogType
      -- ^ The type of log output (default is ApacheLog).

    , displayVersion :: !Bool
      -- ^ Display version?

    , swagger        :: !Bool
      -- ^ Generate swagger.json
    } deriving Show

-- | Description of the format for the log output. Either it is ApacheLog
-- style, or it is a more detailed, but less runtime efficient, format
-- feasible for development.
data LogType = DevLog | ApacheLog
    deriving Show

main :: IO ()
main = do
    -- Capture the command line options.
    opts <- getOptions

    -- Check if the user wants to display the version. If so, display version
    -- and then terminate.
    when (displayVersion opts) $ do
        printf "CSIM API Gateway version %s\n" version
        exitSuccess

    -- Check if the user wants to write a pretty printed 'swagger.json' file.
    -- If to, write it to the current directory and then terminate.
    when (swagger opts) $ do
        LBS.writeFile "swagger.json" prettySwagger
        exitSuccess

    -- Connect to the specified NATS server.
    withNats defaultSettings [natsUri opts] $ \nats' -> do

        -- Create the logger from 'Options'.
        logger <- mkLogger (logType opts) (logDest opts)

        let self = Self { staticDir = Main.staticDir opts
                        , nats      = nats'
                        , tmo       = TmoSec $ timeout opts
                        }

        -- Start Warp and make it serve the application. Run a
        -- request logger as 'Middleware'.
        run (apiPort opts) $ logger (app self)

-- | Current version of the CSIM API Gateway.
version :: String
version = "0.1.0.0"

-- | Generate the 'Options' from the command line.
getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options = info (helper <*> optParser)
               (  fullDesc
               <> progDesc "Start the CSIM API Gateway"
               <> header ("CSIM API Gateway. Version " ++ version)
               )

-- | Parse the command line options.
optParser :: Parser Options
optParser =
    Options <$> strOption
                (  long "nats"
                <> short 'n'
                <> metavar "<NATS URI>"
                <> value "nats://localhost:4222"
                <> help "NATS URI for connecting to NATS server (default: nats://localhost:4222)"
                )
            <*> option auto
                (  long "port"
                <> short 'p'
                <> metavar "<PORT>"
                <> value 8000
                <> help "Network port on which API Gateway listen (default: 8000)"
                )
            <*> strOption
                (  long "root"
                <> short 'r'
                <> metavar "<DIRECTORY>"
                <> value "."
                <> help "Root directory where to find static files (default: '.')"
                )
            <*> option auto
                (  long "timeout"
                <> short 't'
                <> metavar "<SECONDS>"
                <> value 5
                <> help "Timeout duration for requests (default: 5)"
                )
            <*> optional (strOption
                    (  long "logdest"
                    <> short 'l'
                    <> metavar "<FILEPATH>"
                    <> help "Set log file destination (if not set, stdout)"
                    )
                )
            <*> flag ApacheLog DevLog
                (  long "devlog"
                <> short 'd'
                <> help "Set log file format to dev (if not set, Apache)"
                )
            <*> switch
                (  long "version"
                <> short 'v'
                <> help "Display program version"
                )
            <*> switch
                (  long "swagger"
                <> short 's'
                <> help "Generate 'swagger.json' in current directory"
                )

-- | Setup the logger with format and destination
mkLogger :: LogType -> Maybe FilePath -> IO Middleware
mkLogger ApacheLog Nothing =
    mkRequestLogger $ def {outputFormat = Apache FromSocket}

mkLogger ApacheLog (Just filePath) = do
    loggerSet <- newFileLoggerSet defaultBufSize filePath
    mkRequestLogger $ def { outputFormat = Apache FromSocket
                          , destination  = Logger loggerSet
                          }

mkLogger DevLog Nothing = mkRequestLogger def

mkLogger DevLog (Just filePath) = do
    loggerSet <- newFileLoggerSet defaultBufSize filePath
    mkRequestLogger $ def {destination = Logger loggerSet}
