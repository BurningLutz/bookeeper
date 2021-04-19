module Main where


import Protolude
import Data.Pool
import qualified Data.ByteString.Base64 as B64
import Servant
import Servant.Auth.Server
import Network.Wai.Logger
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple (connectPostgreSQL, close)

import Bookeeper.API
import Bookeeper.AppM
import Bookeeper.Server
import Bookeeper.Model


type ContextSet = '[CookieSettings, JWTSettings]

secret :: ByteString
secret = "1mV2A3jU+lmNHtIjKWj5kinJi3Y4qEAZTmj7aGGXRNc04JtH5icto4rCqIKAAVNpmRUup/7jKCjZzsk2faPgXKtHG4hrMLN8rRXYUo+VgKbo/y1NonlUVtYCtfcCfprwjX9TR/hDIUh+WTdNPMJoXcewt5dJ0Fh6yoVPL2Am05NeLmS+Gz2NYle6zgso5IoIhkkmL0Iha4wLXevQdyr0JZaQhPJpzSB8eolOb/E0IsHvxYzdod93ZHzeN9RY7LixGHWyv9QW5koQqn9xl8izcfKRnbMbw4demqwiHs4BZ2BxN94y8AjHEZo/6UfKbLDtdbBrmiDK25MxsU6lXt7XJg=="

main :: IO ()
main = do
  pool <- createPool (connectPostgreSQL "host=localhost dbname=postgres") close 1 60 10

  let
    key = fromSecret $ B64.decodeLenient secret

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

  withStdoutLogger \logger -> do
    let
      settings = setPort 8081 $ setLogger logger defaultSettings

    runSettings settings app
