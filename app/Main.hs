module Main where


import Protolude

import Data.Pool
import Data.Yaml
import qualified Data.Text as T
import qualified Data.ByteString.Base64 as B64
import Control.Monad.Except (liftEither)
import Servant
import Servant.Auth.Server
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Database.PostgreSQL.Simple (connectPostgreSQL, close)

import Bookeeper.API
import Bookeeper.AppM
import Bookeeper.Server
import Bookeeper.Model


type ContextSet = '[CookieSettings, JWTSettings]

main :: IO ()
main = do
  args <- getArgs

  ei <- runExceptT do
    cfgPath <- liftEither $ head args & maybeToRight do
                 putText "ERROR: config file not provided."
                 putText ""
                 putText "Generated jwt secret is:"
                 putText ""

                 putStrLn . B64.encode =<< generateSecret

    decodeFileThrow cfgPath

  ei & either identity runApp

  where
    runApp :: AppConfig -> IO ()
    runApp AppConfig {..} = do
      let connStr = [ maybe "" ("host=" <>)   dbHost
                    , maybe "" ("dbname=" <>) dbName
                    , maybe "" ("username=" <>) dbUser
                    ]
                  & T.intercalate " "
                  & encodeUtf8

      pool <- createPool (connectPostgreSQL connStr) close 1 60 dbPoolMaxConns

      let key          = fromSecret $ B64.decodeLenient (encodeUtf8 jwtSecret)
          jwtSettings_ = defaultJWTSettings key

          env = Env
            { jwtSettings    = jwtSettings_
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

      run port (logStdout app)
