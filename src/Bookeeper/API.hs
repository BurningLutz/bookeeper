module Bookeeper.API
  ( FullAPI
  , UserAPI
  , BookAPI
  ) where


import Protolude

import Data.Vector
import Servant

import Bookeeper.Model


type FullAPI = UserAPI :<|> BookAPI


type UserAPI = "users"
            :> ( Get '[JSON] (Vector User)
            :<|> ReqBody '[JSON] AddUser :> PostNoContent
               )


type BookAPI = "books"
            :> ( Get '[JSON] (Vector Book)
            :<|> Capture "id" Word64 :> DeleteNoContent
               )
