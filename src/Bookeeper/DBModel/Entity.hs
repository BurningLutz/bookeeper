module Bookeeper.DBModel.Entity
  ( Entity'(..)
  , Entity
  , EntityR
  , EntityW
  , pEntity
  ) where


import Protolude

import Data.Time.Clock
import Data.Aeson
import Data.HashMap.Strict
import Data.Profunctor.Product.TH
import Opaleye


data Entity' a b c d = Entity
  { _id        :: a
  , _createdAt :: b
  , _updatedAt :: c
  , _value     :: d
  }
instance (ToJSON a, ToJSON d) => ToJSON (Entity' a b c d) where
  toJSON Entity {..} =
    let val = toJSON _value
        id  = toJSON _id
     in case val of
          Object obj -> Object $ insert "id" id obj
          _          -> object [("id", id), ("value", val)]

$(makeAdaptorAndInstanceInferrable' ''Entity')
type Entity a = Entity' Int64 UTCTime UTCTime a
type EntityR a = Entity' (Field SqlInt8)
                         (Field SqlTimestamptz)
                         (Field SqlTimestamptz)
                         a
type EntityW a = Entity' (Maybe (Field SqlInt8))
                         (Maybe (Field SqlTimestamptz))
                         (Maybe (Field SqlTimestamptz))
                         a
