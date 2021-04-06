module Bookeeper.API
  ( UserAPI
  ) where


import Protolude

import Servant
import Data.Vector

import Bookeeper.Model


type UserAPI = "users" :> ( Get '[JSON] (Vector User) :<|> Post '[JSON] Bool)
