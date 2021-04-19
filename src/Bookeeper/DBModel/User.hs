module Bookeeper.DBModel.User
  ( User'(..)
  , User
  , UserR
  , UserW
  , pUser
  ) where


import Protolude

import Data.Aeson.TH
import Data.Profunctor.Product.TH
import Opaleye

import Bookeeper.Util
import Bookeeper.DBModel.Entity


data User' a b c = User
  { _nickname :: a
  , _isVip    :: b
  , _age      :: c
  }
$(deriveJSON jsonOptions ''User')
$(makeAdaptorAndInstanceInferrable' ''User')
type User = Entity (User' Text Bool (Maybe Int32))
type UserF =  User' (Field SqlText)
                    (Field SqlBool)
                    (FieldNullable SqlInt4)
type UserR = EntityR UserF
type UserW = EntityW UserF
