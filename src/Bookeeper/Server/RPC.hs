module Bookeeper.Server.RPC
  ( rpcServer
  ) where


import Protolude hiding (decodeUtf8)

import Data.Pool
import Data.Time.Clock
import Data.Text.Lazy.Encoding
import Control.Lens
import Opaleye
import Servant
import Servant.Auth.Server

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.Model
import Bookeeper.Query


rpcServer :: ServerT RPCAPI AppM
rpcServer = adminLogin
       :<|> userLogin

  where
    adminLogin :: AdminLogin -> AppM (WithAccessToken ClaimAdmin)
    adminLogin AdminLogin {..} = do
      Env { pool, jwtSettings } <- ask

      mAdmin :: Maybe Admin <- liftIO $ withResource pool \conn -> do
        listToMaybe <$> runSelect conn do
          admin <- selectTable admins

          viaLateral restrict ( (admin^.value.nickname .== toFields _nickname)
                            .&& (admin^.value.password .== toFields _password)
                              )

          pure admin

      mAdmin & maybe (throwError err401) \_ -> do
        now <- liftIO getCurrentTime

        let
          claimAdmin = ClaimAdmin { _nickname }
          expiresAt = addUTCTime (7 * nominalDay) now

        eiJwt <- liftIO $ makeJWT claimAdmin jwtSettings (Just expiresAt)

        eiJwt & either (const $ throwError err500) \jwt -> do
          pure WithAccessToken
            { _access_token = toStrict $ decodeUtf8 jwt
            , _payload      = claimAdmin
            }

    userLogin :: UserLogin -> AppM (WithAccessToken ClaimUser)
    userLogin UserLogin {..} = do
      Env { pool, jwtSettings } <- ask

      mUser :: Maybe User <- liftIO $ withResource pool \conn -> do
        listToMaybe <$> runSelect conn do
          user <- selectTable users

          viaLateral restrict (user^.value.nickname .== toFields _nickname)

          pure user

      mUser & maybe (throwError err401) \user -> do
        now <- liftIO getCurrentTime

        let
          claimUser = ClaimUser { _nickname, _isVip = user^.value.isVip }
          expiresAt = addUTCTime (7 * nominalDay) now

        eiJwt <- liftIO $ makeJWT claimUser jwtSettings (Just expiresAt)

        eiJwt & either (const $ throwError err500) \jwt -> do
          pure WithAccessToken
            { _access_token = toStrict $ decodeUtf8 jwt
            , _payload      = claimUser
            }
