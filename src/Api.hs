{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | Entry module where the 'CsimAPI' is defined.
module Api
    ( CsimAPI
    , app
    ) where

import Servant

-- | Csim API type ...
type CsimAPI =
    "foo" :> Get '[JSON] [Int]

-- | The application handling the 'CsimAPI'. 
app :: Application
app = serve apiProxy apiRouter

apiProxy :: Proxy CsimAPI
apiProxy = Proxy

apiRouter :: Server CsimAPI
apiRouter = return [1]
