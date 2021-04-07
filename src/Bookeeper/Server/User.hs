module Bookeeper.Server.User
  ( userServer
  ) where


import Protolude
import Servant
import Data.Vector as V

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.Model


userServer :: ServerT UserAPI AppM
userServer = getUsers
    :<|> addUser

  where
    getUsers :: AppM (Vector User)
    getUsers = do
      pure V.empty

    addUser :: AddUser -> AppM NoContent
    addUser _ = do
      pure NoContent
