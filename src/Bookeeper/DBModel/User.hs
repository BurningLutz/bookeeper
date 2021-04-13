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


data User' a b c d = User
  { nickname :: a
  , password :: b
  , age      :: c
  , isVip    :: d
  }
$(deriveJSON defaultOptions ''User')
$(makeAdaptorAndInstanceInferrable' ''User')
type User = Entity (User' Text Text Int32 Bool)
type UserR = EntityR ( User' (Field SqlText)
                             ()
                             (Field SqlInt4)
                             (Field SqlBool)
                     )
type UserW = EntityW ( User' (Field SqlText)
                             (Field SqlText)
                             (Field SqlInt4)
                             (Field SqlBool)
                     )

users :: Table UserW UserR
users = table "users" $ withEntity $ pUser User
  { nickname = tableField "nickname"
  , password = writeOnlyTableField "password"
  , age      = tableField "age"
  , isVip    = tableField "is_vip"
  }
