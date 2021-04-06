module Bookeeper.Model
  ( User(..)
  , AddUser(..)
  , AdminLevel(..)
  , Admin(..)
  , Book(..)
  , BorrowingStatus(..)
  , Borrowing(..)

  , id
  , nickname
  , age
  , isVip
  , level
  , book
  , user
  , status
  , date
  ) where


import Protolude

import Data.Time
import Data.Aeson.TH
import Control.Lens

import Bookeeper.Util


data User = User
  { _id       :: Word64
  , _nickname :: Text
  , _age      :: Word8
  , _isVip    :: Bool
  }

deriveJSON jsonOptions ''User
makeFieldsNoPrefix ''User


data AddUser = AddUser
  { _nickname :: Text
  , _age      :: Word8
  , _isVip    :: Bool
  }

deriveJSON jsonOptions ''AddUser
makeFieldsNoPrefix ''AddUser


data AdminLevel = Low | Mid | High

deriveJSON jsonOptions ''AdminLevel

data Admin = Admin
  { _nickname :: Text
  , _level    :: AdminLevel
  }

deriveJSON jsonOptions ''Admin
makeFieldsNoPrefix ''Admin


data Book = Book
  { _sn     :: Text
  , _title  :: Text
  , _authur :: Text
  }

deriveJSON jsonOptions ''Book

data BorrowingStatus = Pending | Approved | Returned

deriveJSON jsonOptions ''BorrowingStatus

data Borrowing = Borrowing
  { _book   :: Book
  , _user   :: User
  , _date   :: UTCTime
  , _status :: BorrowingStatus
  }

deriveJSON jsonOptions ''Borrowing
makeFieldsNoPrefix ''Borrowing
