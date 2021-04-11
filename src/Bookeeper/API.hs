module Bookeeper.API
  ( FullAPI
  , UserAPI
  , BookAPI
  , RPCAPI
  ) where


import Protolude

import Data.Vector
import Servant
import Servant.Auth.Server

import Bookeeper.APIModel


type AllowUser  = Auth '[JWT] User
type AllowAdmin = Auth '[JWT] ClaimAdmin

type FullAPI = UserAPI :<|> RPCAPI

type UserAPI = "users"
                 :> AllowAdmin
                 :> Get '[JSON] (Vector User)
          :<|> "users"
                 :> ReqBody '[JSON] AddUser
                 :> PostCreated '[JSON] User

type BookAPI = "books"
                 :> Get '[JSON] (Vector Book)
          :<|> "books"
                 :> Capture "id" Word64
                 :> PostCreated '[JSON] Book

type RPCAPI = "rpc"
           :> ( "admin-login"
                  :> ReqBody '[JSON] AdminLogin
                  :> Post '[JSON] (WithAccessToken Admin)
           :<|> "user-login"
                  :> ReqBody '[JSON] AddUser
                  :> Post '[JSON] (WithAccessToken User)
              )

-- type BorrowingAPI = "borrowing" :> AllowUser :> Get '[JSON] (Vector Borrowing)
