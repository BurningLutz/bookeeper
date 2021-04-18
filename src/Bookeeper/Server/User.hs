module Bookeeper.Server.User
  ( userServer
  ) where


import Protolude

import Data.Pool
import Opaleye
import Servant
import Servant.Auth.Server

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.APIModel
import Bookeeper.DBModel
import Bookeeper.Data


userServer :: ServerT UserAPI AppM
userServer = adminUserServer
        :<|> currentUserServer

  where
    adminUserServer (Authenticated _) = getUsers :<|> addUser :<|> updUser
      where
        getUsers :: AppM [User]
        getUsers = do
          Env { pool } <- ask

          liftIO $ withResource pool \conn -> do
            runSelect conn (selectTable users)

        addUser :: AddUser -> AppM NoContent
        addUser AddUser {..} = do
          Env { pool } <- ask

          let
            user = toFields $ wrapEntity User { nickname, age, isVip }

          liftIO $ withResource pool \conn -> do
            runInsert_ conn Insert
              { iTable = users
              , iRows = [user]
              , iReturning = rCount
              , iOnConflict = Nothing
              }

          pure NoContent

        updUser :: Int64 -> UpdUser -> AppM NoContent
        updUser uid UpdUser {..} = do
          Env { pool } <- ask

          liftIO $ withResource pool \conn -> do
            runUpdate_ conn Update
              { uTable = users
              , uWhere = \Entity { id } -> id .== toFields uid
              , uUpdateWith = updEntity undefined
              , uReturning = rCount
              }

          pure NoContent
    adminUserServer _ = throwAll err401

    currentUserServer (Authenticated _) = currentUser
      where
        currentUser :: AppM User
        currentUser = undefined
    currentUserServer _ = throwAll err401
