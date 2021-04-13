module Bookeeper.Server.RPC
  ( rpcServer
  ) where


import Protolude hiding (decodeUtf8)
import Servant
import Servant.Auth.Server
import Data.Text.Lazy.Encoding

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.APIModel
import Bookeeper.DBModel
import Bookeeper.Util


rpcServer :: ServerT RPCAPI AppM
rpcServer = adminLogin
       :<|> userLogin

  where
    adminLogin :: AdminLogin -> AppM (WithAccessToken ClaimAdmin)
    adminLogin AdminLogin { nickname, password } = do
      unless (nickname == "Lutz" && password == "123456")
             (throwError err401)

      Env { jwtSettings } <- ask

      let
        claimAdmin =
          ClaimAdmin
            { nickname
            , level = High
            }
      eiJwt <- liftIO $ makeJWT claimAdmin jwtSettings Nothing

      case eiJwt of
        Left _ -> throwError err500
        Right jwt -> do
          pure $
            WithAccessToken
              { access_token = toStrict $ decodeUtf8 jwt
              , payload =
                  ClaimAdmin
                    { nickname
                    , level = High
                    }
              }

    userLogin = undefined
