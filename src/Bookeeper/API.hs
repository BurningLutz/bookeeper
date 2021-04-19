module Bookeeper.API
  ( FullAPI
  , UserAPI
  , BookAPI
  , RPCAPI
  ) where


import Protolude

import Servant
import Servant.Auth.Server

import Bookeeper.Model


type AllowUser  = Auth '[JWT] ClaimUser
type AllowAdmin = Auth '[JWT] ClaimAdmin

type FullAPI = "users" :> UserAPI
          :<|> "rpc" :> RPCAPI

type UserAPI = AllowAdmin :> AdminUserAPI
          :<|> AllowUser :> CurrentUserAPI

type AdminUserAPI = Get '[JSON] [User]
               :<|> ReqBody '[JSON] AddUser
                     :> Verb 'POST 204 '[JSON] NoContent
               :<|> Capture "id" Int64
                     :> ReqBody '[JSON] UpdUser
                     :> Verb 'PATCH 204 '[JSON] NoContent

type CurrentUserAPI = "me" :> Get '[JSON] User

type BookAPI = "books"
                 :> Get '[JSON] [Book]
          :<|> "books"
                 :> Capture "id" Word64
                 :> PostCreated '[JSON] Book

-- type BorrowingAPI = "borrowing" :> AllowUser :> Get '[JSON] (Vector Borrowing)


type RPCAPI = "admin-login"
                :> ReqBody '[JSON] AdminLogin
                :> Post '[JSON] (WithAccessToken ClaimAdmin)
         :<|> "user-login"
                :> ReqBody '[JSON] UserLogin
                :> Post '[JSON] (WithAccessToken ClaimUser)
