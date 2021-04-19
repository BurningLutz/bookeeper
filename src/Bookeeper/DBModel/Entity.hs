module Bookeeper.DBModel.Entity
  ( Entity'(..)
  , Entity
  , EntityR
  , EntityW
  , pEntity
  ) where


import Protolude

import Data.Time.Clock
import Data.Aeson.TH
import Data.Profunctor.Product.TH
import Opaleye

import Bookeeper.Util


data Entity' a b c d = Entity
  { _id        :: a
  , _createdAt :: b
  , _updatedAt :: c
  , _value     :: d
  }
$(deriveJSON jsonOptions ''Entity')
$(makeAdaptorAndInstanceInferrable' ''Entity')
type Entity a = Entity' Int64 UTCTime UTCTime a
type EntityR a = Entity' (Field SqlInt8)
                         (Field SqlTimestamptz)
                         (Field SqlTimestamptz)
                         a
type EntityW a = Entity' ()
                         ()
                         (Maybe (Field SqlTimestamptz))
                         a
