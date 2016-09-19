module Main
    ( main
    ) where

import Options.Applicative

-- | Command line options.
data Options = Options
    { natsUri        :: !String
      -- ^ The URI for the NATS server to connect to.

    , apiPort        :: !Int
      -- ^ The network port on which the API Gateway shall listen.

    , rootDir        :: !FilePath
      -- ^ The root directory for static files.

    , logDest        :: !(Maybe FilePath)
      -- ^ A specified log destination (otherwise stdout).

    , logType        :: !LogType
      -- ^ The type of log output (default is ApacheLog).

    , displayVersion :: !Bool
      -- ^ Display version?
    } deriving Show

-- | Description of the format for the log output. Either it is ApacheLog
-- style, or it is a more detailed, but less runtime efficient, format
-- feasible for development.
data LogType = DevLog | ApacheLog
    deriving Show

main :: IO ()
main = print =<< getOptions

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
                <> help "Root directory where to find static files (default: .)"                
                )
            <*> (optional $ strOption
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