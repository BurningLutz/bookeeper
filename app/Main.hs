module Main where


import Protolude
import Servant
import Network.Wai.Handler.Warp

import Bookeeper.API
import Bookeeper.AppM
import Bookeeper.Server


appServer :: Server FullAPI
appServer = hoistServer (Proxy @FullAPI) runAppM server

app :: Application
app = serve (Proxy @FullAPI) appServer

main :: IO ()
main = run 8081 app
