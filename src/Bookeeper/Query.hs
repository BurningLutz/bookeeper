module Bookeeper.Query
  ( withEntity
  , newEntity
  , modEntity
  , uniqError
  , admins
  , users
  , books
  , borrowings
  ) where


import Protolude

import Data.Time
import Data.Profunctor.Product.Default
import Servant
import Opaleye
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors

import Bookeeper.Model


withEntity :: TableFields w r -> TableFields (EntityW w) (EntityR r)
withEntity _value = pEntity $ Entity
  { _id        = readOnlyTableField "id"
  , _createdAt = readOnlyTableField "created_at"
  , _updatedAt = tableField "updated_at"
  , _value
  }

newEntity :: Default ToFields entity entityW => entity -> EntityW entityW
newEntity _value = toFields Entity
  { _id = ()
  , _createdAt = ()
  , _updatedAt = Nothing @UTCTime
  , _value
  }

modEntity :: (entityR -> entityW) -> EntityR entityR -> EntityW entityW
modEntity conv entity@Entity { _value } = entity
  { _id = ()
  , _createdAt = ()
  , _updatedAt = Nothing
  , _value = conv _value
  }

uniqError :: ByteString -> SqlError -> ConstraintViolation -> IO (Either ServerError a)
uniqError s0 _ (UniqueViolation s1) | s0 == s1 = pure $ Left err409 { errBody = "already exists" }
uniqError _  e _                               = throwIO e

admins :: Table AdminW AdminR
admins = table "admins" $ withEntity $ pAdmin Admin
  { _nickname = tableField "nickname"
  , _password = tableField "password"
  }

users :: Table UserW UserR
users = table "users" $ withEntity $ pUser User
  { _nickname = tableField "nickname"
  , _age      = tableField "age"
  , _isVip    = tableField "is_vip"
  }

books :: Table BookW BookR
books = table "books" $ withEntity $ pBook Book
  { _sn     = tableField "sn"
  , _title  = tableField "title"
  , _author = tableField "authur"
  }

borrowings :: Table BorrowingW BorrowingR
borrowings = table "borrowings" $ withEntity $ pBorrowing Borrowing
  { _bookId = readOnlyTableField "book_id"
  , _userId = readOnlyTableField "user_id"
  , _date   = tableField "date"
  , _status = tableField "status"
  }
