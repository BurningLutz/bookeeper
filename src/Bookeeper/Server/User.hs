module Bookeeper.Server.User
  ( userServer
  ) where


import Protolude

import Data.Pool
import Control.Lens
import Opaleye
import Servant
import Servant.Auth.Server

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.Model
import Bookeeper.Query


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
            user = newEntity User { _nickname, _age, _isVip }

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
              , uWhere = \u -> u^.id .== toFields uid
              , uUpdateWith = modEntity ( (age .~ toFields _age)
                                        . (isVip .~ toFields _isVip)
                                        )
              , uReturning = rCount
              }

          pure NoContent
    adminUserServer _ = throwAll err401

    currentUserServer (Authenticated ClaimUser {..}) = currentUser
      where
        currentUser :: AppM User
        currentUser = do
          Env { pool } <- ask

          mUser :: Maybe User <- liftIO $ withResource pool \conn -> do
            listToMaybe <$> runSelect conn do
              user <- selectTable users

              viaLateral restrict (user^.value.nickname .== toFields _nickname)

              pure user

          mUser & maybe (throwError err401) pure
    currentUserServer _ = throwAll err401
