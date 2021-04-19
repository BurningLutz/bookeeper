module Bookeeper.API
  ( FullAPI
  , UserAPI
  , BookAPI
  , BorrowingAPI
  , RPCAPI
  ) where


import Protolude

import Servant
import Servant.Auth.Server

import Bookeeper.Model


type AllowUser  = Auth '[JWT] ClaimUser
type AllowAdmin = Auth '[JWT] ClaimAdmin


type FullAPI = "users"      :> UserAPI
          :<|> "books"      :> BookAPI
          :<|> "borrowings" :> BorrowingAPI
          :<|> "rpc"        :> RPCAPI


type UserAPI = AllowAdmin :> AdminUserAPI
          :<|> AllowUser :> CurrentUserAPI

type AdminUserAPI = Get '[JSON] [User]
               :<|> ReqBody '[JSON] AddUser
                      :> Verb 'POST 204 '[JSON] NoContent
               :<|> Capture "id" Int64
                      :> ReqBody '[JSON] SetUser
                      :> Verb 'PUT 204 '[JSON] NoContent

type CurrentUserAPI = "me" :> Get '[JSON] User


type BookAPI = AllowAdmin :> AdminBookAPI
          :<|> PublicBookAPI

type AdminBookAPI = ReqBody '[JSON] AddBook
                      :> PostCreated '[JSON] Int64
               :<|> Capture "id" Int64
                      :> ReqBody '[JSON] SetBook
                      :> Verb 'PUT 204 '[JSON] NoContent
               :<|> Capture "id" Int64
                      :> Verb 'DELETE 204 '[JSON] NoContent

type PublicBookAPI = Get '[JSON] [Book]


type BorrowingAPI = AllowAdmin :> AdminBorrowingAPI
               :<|> AllowUser :> UserBorrowingAPI

type AdminBorrowingAPI = Get '[JSON] [BorrowingDetail]
                    :<|> Capture "id" Int64
                           :> ReqBody '[JSON] SetBorrowing
                           :> Verb 'PUT 204 '[JSON] NoContent

type UserBorrowingAPI = ReqBody '[JSON] AddBorrowing
                          :> PostCreated '[JSON] Int64
                    :<|> "me"
                          :> Get '[JSON] [Borrowing]


type RPCAPI = "admin-login"
                 :> ReqBody '[JSON] AdminLogin
                 :> Post '[JSON] (WithAccessToken ClaimAdmin)
         :<|> "user-login"
                 :> ReqBody '[JSON] UserLogin
                 :> Post '[JSON] (WithAccessToken ClaimUser)
