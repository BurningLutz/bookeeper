module Main where


import Protolude
import Data.Pool
import Servant
import Servant.Auth.Server
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple (connectPostgreSQL, close)

import Bookeeper.API
import Bookeeper.AppM
import Bookeeper.Server
import Bookeeper.Model


type ContextSet = '[CookieSettings, JWTSettings]

main :: IO ()
main = do
  key  <- generateKey
  pool <- createPool (connectPostgreSQL "host=localhost dbname=postgres") close 1 60 10

  let
    jwtSettings_ :: JWTSettings
    jwtSettings_ = defaultJWTSettings key

    env :: Env
    env = Env { jwtSettings    = jwtSettings_
              , cookieSettings = defaultCookieSettings
              , pool
              }

    ctx :: Context ContextSet
    ctx = defaultCookieSettings
       :. jwtSettings_
       :. EmptyContext

    appServer :: Server FullAPI
    appServer = hoistServerWithContext
                  (Proxy @FullAPI)
                  (Proxy @ContextSet)
                  (runAppM env)
                  server

    app :: Application
    app = serveWithContext
            (Proxy @FullAPI)
            ctx
            appServer

  run 8081 app
