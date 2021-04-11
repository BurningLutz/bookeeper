module Bookeeper.Server.User
  ( userServer
  ) where


import Protolude
import Servant
import Servant.Auth.Server
import Data.Vector as V

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.APIModel


userServer :: ServerT UserAPI AppM
userServer = getUsers
        :<|> addUser

  where
    getUsers :: AuthResult ClaimAdmin -> AppM (Vector User)
    getUsers (Authenticated _) = do
      pure V.empty
    getUsers _ = throwAll err401

    addUser :: AddUser -> AppM User
    addUser _ = undefined
