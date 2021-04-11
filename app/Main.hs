module Main where


import Protolude
import Servant
import Servant.Auth.Server
import Network.Wai.Handler.Warp

import Bookeeper.API
import Bookeeper.AppM
import Bookeeper.Server
import Bookeeper.APIModel


type ContextSet = '[CookieSettings, JWTSettings]

main :: IO ()
main = do
  key <- generateKey

  let
    jwtSettings_ :: JWTSettings
    jwtSettings_ = defaultJWTSettings key

    env :: Env
    env = Env { jwtSettings    = jwtSettings_
              , cookieSettings = defaultCookieSettings
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
