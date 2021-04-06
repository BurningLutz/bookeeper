module Main where


import Protolude hiding (Handler)

import Servant
import Data.Vector
import Control.Monad.RWS
import Network.Wai.Handler.Warp

import Bookeeper.AppM


type UserAPI = "users" :> Get '[JSON] (Vector Text)

server :: ServerT UserAPI AppM
server = do
  tell ["g"]
  pure ["f", "g"]

app :: Application
app = serve @UserAPI Proxy (hoistServer @UserAPI Proxy runAppM server)

main :: IO ()
main = run 8081 app
