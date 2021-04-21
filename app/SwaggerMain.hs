module Main where


import Protolude

import Servant.Server
import Servant.Swagger
import Servant.Swagger.UI.ReDoc
import Servant.Auth.Swagger ()
import Network.Wai.Handler.Warp

import Bookeeper.API


type API = SwaggerSchemaUI "redoc" "openapi.json"

server :: Server API
server = redocSchemaUIServer (toSwagger (Proxy @FullAPI))

app :: Application
app = serve (Proxy @API) server

main :: IO ()
main = do
  run 8081 app
