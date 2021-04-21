module Bookeeper.DBModel.User
  ( User'(..)
  , User
  , UserR
  , UserW
  , pUser
  ) where


import Protolude

import Data.Swagger
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
  deriving stock (Generic)
$(deriveJSON jsonOptions ''User')
$(makeAdaptorAndInstanceInferrable' ''User')
type UserE = User' Text Bool (Maybe Int32)
instance ToSchema UserE

type User  = Entity UserE
type UserF =  User' (Field SqlText)
                    (Field SqlBool)
                    (FieldNullable SqlInt4)
type UserR = EntityR UserF
type UserW = EntityW UserF
