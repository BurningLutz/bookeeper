module Bookeeper.DBModel.User
  ( User'(..)
  , User
  , UserR
  , UserW
  , pUser

  , users
  ) where


import Protolude

import Opaleye
import Data.Aeson.TH
import Data.Profunctor.Product.TH

import Bookeeper.DBModel.Entity
import Bookeeper.Util


data User' a b c = User
  { nickname :: a
  , age      :: b
  , isVip    :: c
  }
$(deriveJSON jsonOptions ''User')
$(makeAdaptorAndInstanceInferrable' ''User')
type User = Entity (User' Text Int32 Bool)
type UserF =  User' (Field SqlText)
                    (Field SqlInt4)
                    (Field SqlBool)
type UserR = EntityR UserF
type UserW = EntityW UserF

users :: Table UserW UserR
users = table "users" $ withEntity $ pUser User
  { nickname = tableField "nickname"
  , age      = tableField "age"
  , isVip    = tableField "is_vip"
  }
