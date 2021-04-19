module Bookeeper.Server.User
  ( userServer
  ) where


import Protolude

import Data.Pool
import Control.Lens
import Opaleye
import Servant
import Servant.Auth.Server
import Database.PostgreSQL.Simple.Errors

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.Model
import Bookeeper.Query


userServer :: ServerT UserAPI AppM
userServer = adminUserServer
        :<|> currentUserServer

  where
    adminUserServer (Authenticated ClaimAdmin {}) = getUsers :<|> addUser :<|> setUser
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
            newUser = newEntity User { _nickname, _age, _isVip }

          ei <- liftIO $ withResource pool \conn -> do
            catchViolation (uniqError "users_nickname_key") do
              Right <$> runInsert_ conn Insert
                { iTable = users
                , iRows = [newUser]
                , iReturning = rCount
                , iOnConflict = Nothing
                }

          ei & either throwError (const $ pure NoContent)

        setUser :: Int64 -> SetUser -> AppM NoContent
        setUser uid SetUser {..} = do
          Env { pool } <- ask

          n <- liftIO $ withResource pool \conn -> do
            runUpdate_ conn Update
              { uTable = users
              , uWhere = \u -> u^.id .== toFields uid
              , uUpdateWith = modEntity ( (age .~ toFields _age)
                                        . (isVip .~ toFields _isVip)
                                        )
              , uReturning = rCount
              }

          if n > 0
             then pure NoContent
             else throwError err404

    adminUserServer _ = throwAll err401

    currentUserServer (Authenticated ClaimUser {..}) = currentUser
      where
        currentUser :: AppM User
        currentUser = do
          Env { pool } <- ask

          mUser :: Maybe User <- liftIO $ withResource pool \conn -> do
            listToMaybe <$> runSelect conn do
              userR <- selectTable users

              viaLateral restrict (userR^.value.nickname .== toFields _nickname)

              pure userR

          mUser & maybe (throwError err401) pure
    currentUserServer _ = throwAll err401
