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
          :<|> AllowUser  :> CurrentUserAPI

type AdminUserAPI = Summary "Get All Users"
                      :> Get '[JSON] [User]
               :<|> Summary "Add a User"
                      :> ReqBody '[JSON] AddUser
                      :> Verb 'POST 204 '[JSON] NoContent
               :<|> Summary "Update a User"
                      :> Capture "id" Int64
                      :> ReqBody '[JSON] SetUser
                      :> Verb 'PUT 204 '[JSON] NoContent

type CurrentUserAPI = Summary "Get Current User"
                        :> "me"
                        :> Get '[JSON] User


type BookAPI = AllowAdmin :> AdminBookAPI
          :<|> PublicBookAPI

type AdminBookAPI = Summary "Add a Book"
                      :> ReqBody '[JSON] AddBook
                      :> PostCreated '[JSON] Int64
               :<|> Summary "Update a Book"
                      :> Capture "id" Int64
                      :> ReqBody '[JSON] SetBook
                      :> Verb 'PUT 204 '[JSON] NoContent
               :<|> Summary "Delete a Book"
                      :> Capture "id" Int64
                      :> Verb 'DELETE 204 '[JSON] NoContent

type PublicBookAPI = Summary "Get All Books"
                       :> Get '[JSON] [Book]


type BorrowingAPI = AllowAdmin :> AdminBorrowingAPI
               :<|> AllowUser :> UserBorrowingAPI

type AdminBorrowingAPI = Summary "Get All Borrowing Details"
                           :> Get '[JSON] [BorrowingDetail]
                    :<|> Summary "Update a Borrowing"
                           :> Capture "id" Int64
                           :> ReqBody '[JSON] SetBorrowing
                           :> Verb 'PUT 204 '[JSON] NoContent

type UserBorrowingAPI = Summary "Borrow a Book"
                          :> ReqBody '[JSON] AddBorrowing
                          :> PostCreated '[JSON] Int64
                    :<|> Summary "Get All My Borrowed Books"
                          :> "me"
                          :> Get '[JSON] [Borrowing]


type RPCAPI = Summary "Admin Login"
                 :> "admin-login"
                 :> ReqBody '[JSON] AdminLogin
                 :> Post '[JSON] (WithAccessToken ClaimAdmin)
         :<|> Summary "User Login"
                 :> "user-login"
                 :> ReqBody '[JSON] UserLogin
                 :> Post '[JSON] (WithAccessToken ClaimUser)
