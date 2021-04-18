module Bookeeper.DBModel.Entity
  ( Entity'(..)
  , Entity
  , EntityR
  , EntityW
  , pEntity

  , withEntity
  , wrapEntity
  , updEntity
  ) where


import Protolude

import Opaleye
import Data.Time
import Data.Aeson.TH
import Data.Profunctor.Product.TH

import Bookeeper.Util


data Entity' a b c d = Entity
  { id        :: a
  , createdAt :: b
  , updatedAt :: c
  , entity    :: d
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

withEntity :: TableFields w r -> TableFields (EntityW w) (EntityR r)
withEntity entity = pEntity $ Entity
  { id        = readOnlyTableField "id"
  , createdAt = readOnlyTableField "created_at"
  , updatedAt = tableField "updated_at"
  , entity
  }

wrapEntity :: entity -> EntityW entity
wrapEntity entity = Entity
  { id = ()
  , createdAt = ()
  , updatedAt = Nothing
  , entity
  }

updEntity :: (entity -> entity) -> EntityR entity -> EntityW entity
updEntity conv entity'@Entity { entity } = entity'
  { id = ()
  , createdAt = ()
  , updatedAt = Nothing
  , entity = conv entity
  }
