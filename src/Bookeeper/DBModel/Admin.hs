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

import Bookeeper.Util
import Bookeeper.DBModel.Entity


data Admin' a b = Admin
  { nickname :: a
  , password :: b
  }
$(deriveJSON jsonOptions ''Admin')
$(makeAdaptorAndInstanceInferrable' ''Admin')
type Admin = Entity (Admin' Text Text)
type AdminF = Admin' (Field SqlText)
                     (Field SqlText)

type AdminR = EntityR AdminF
type AdminW = EntityW AdminF

admins :: Table AdminW AdminR
admins = table "admins" $ withEntity $ pAdmin Admin
  { nickname = tableField "nickname"
  , password = tableField "password"
  }
