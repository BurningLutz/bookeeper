module Bookeeper.DBModel.Admin
  ( Admin'(..)
  , Admin
  , AdminR
  , AdminW
  , pAdmin
  ) where


import Protolude

import Data.Aeson.TH
import Data.Profunctor.Product.TH
import Opaleye

import Bookeeper.Util
import Bookeeper.DBModel.Entity


data Admin' a b = Admin
  { _nickname :: a
  , _password :: b
  }
$(deriveJSON jsonOptions ''Admin')
$(makeAdaptorAndInstanceInferrable' ''Admin')
type Admin = Entity (Admin' Text Text)
type AdminF = Admin' (Field SqlText)
                     (Field SqlText)

type AdminR = EntityR AdminF
type AdminW = EntityW AdminF
