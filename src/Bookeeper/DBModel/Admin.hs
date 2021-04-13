module Bookeeper.DBModel.Admin
  ( Admin'(..)
  , Admin
  , AdminR
  , AdminW
  , pAdmin

  , admins
  ) where


import Protolude

import Opaleye
import Data.Aeson.TH
import Data.Profunctor.Product.TH

import Bookeeper.DBModel.Entity
import Bookeeper.Util


data Admin' a b c = Admin
  { nickname :: a
  , password :: b
  , level    :: c
  }
$(deriveJSON defaultOptions ''Admin')
$(makeAdaptorAndInstanceInferrable' ''Admin')
type Admin = Entity (Admin' Text Text AdminLevel)
type AdminR = EntityR ( Admin' (Field SqlText)
                               ()
                               (Field AdminLevel)
                     )
type AdminW = EntityW ( Admin' (Field SqlText)
                               (Field SqlText)
                               (Field AdminLevel)
                     )

admins :: Table AdminW AdminR
admins = table "admins" $ withEntity $ pAdmin Admin
  { nickname = tableField "nickname"
  , password = writeOnlyTableField "password"
  , level    = tableField "level"
  }
