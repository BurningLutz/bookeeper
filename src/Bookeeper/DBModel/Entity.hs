module Bookeeper.DBModel.Entity
  ( Entity'(..)
  , Entity
  , EntityR
  , EntityW
  , pEntity

  , withEntity
  ) where


import Protolude

import Opaleye
import Data.Time
import Data.Aeson.TH
import Data.Profunctor.Product.TH


data Entity' a b c d = Entity
  { id        :: a
  , createdAt :: b
  , updatedAt :: c
  , entity    :: d
  }
$(deriveJSON defaultOptions ''Entity')
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

withEntity :: TableFields w r -> TableFields (EntityW w) (EntityR r)
withEntity entity = pEntity $ Entity
  { id        = readOnlyTableField "id"
  , createdAt = readOnlyTableField "created_at"
  , updatedAt = tableField "updated_at"
  , entity
  }
