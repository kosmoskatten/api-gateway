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

    , logDest        :: !(Maybe FilePath)
      -- ^ A specified log destination (otherwise stdout).

    , logType        :: !LogType
      -- ^ The type of log output (default is ApacheLog).

    , displayVersion :: !Bool
      -- ^ Display version?
    } deriving Show

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
                <> help "NATS URI for connecting to NATS server"
                )
            <*> option auto
                (  long "port"
                <> short 'p'
                <> metavar "<PORT>"
                <> help "Network port on which API Gateway listen"
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
