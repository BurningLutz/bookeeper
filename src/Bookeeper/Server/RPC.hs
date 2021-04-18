module Bookeeper.Server.RPC
  ( rpcServer
  ) where


import Protolude hiding (decodeUtf8)

import Data.Pool
import Data.Time.Clock
import Data.Text.Lazy.Encoding
import Opaleye
import Servant
import Servant.Auth.Server

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.APIModel
import Bookeeper.DBModel
import Bookeeper.Data


rpcServer :: ServerT RPCAPI AppM
rpcServer = adminLogin
       :<|> userLogin

  where
    adminLogin :: AdminLogin -> AppM (WithAccessToken ClaimAdmin)
    adminLogin AdminLogin { nickname = nickname_, password = password_ } = do
      Env { pool, jwtSettings } <- ask

      mAdmin :: Maybe Admin <- liftIO $ withResource pool \conn -> do
        listToMaybe <$> runSelect conn do
          admin@Entity { entity = Admin {..} } <- selectTable admins

          viaLateral restrict ( nickname .== toFields nickname_
                            .&& password .== toFields password_
                              )

          pure admin

      mAdmin & maybe (throwError err401) \Entity { entity = Admin {..} } -> do
        now <- liftIO getCurrentTime

        let
          claimAdmin = ClaimAdmin { nickname }
          expiresAt = addUTCTime (7 * nominalDay) now

        eiJwt <- liftIO $ makeJWT claimAdmin jwtSettings (Just expiresAt)

        eiJwt & either (\_ -> throwError err500) \jwt -> do
          pure WithAccessToken
            { access_token = toStrict $ decodeUtf8 jwt
            , payload      = claimAdmin
            }

    userLogin = undefined
