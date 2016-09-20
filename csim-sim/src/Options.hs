module Options
    ( Options (..)
    , getOptions
    ) where

import Options.Applicative

-- | Command line options.
data Options = Options
    { natsUri        :: !String
      -- ^ The URI for the NATS server to connect to.
    } deriving Show

-- | Generate the 'Options' from the command line.
getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options = info (helper <*> optParser)
               (  fullDesc
               <> progDesc "Start the simulated component"
               <> header ("Simulated CSIM component")
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
