module Bookeeper.DBModel.Borrowing
  ( Borrowing'(..)
  , Borrowing
  , BorrowingDetail
  , BorrowingR
  , BorrowingW
  , BorrowingStatus(..)
  , pBorrowing
  ) where


import Protolude

import Data.Time
import Data.Aeson.TH
import Data.Profunctor.Product.TH
import Opaleye

import Bookeeper.Data.SqlEnum
import Bookeeper.Util
import Bookeeper.DBModel.Entity
import Bookeeper.DBModel.Book
import Bookeeper.DBModel.User


data BorrowingStatus = Pending | Approved | Returned
  deriving stock (Show, Read)
$(deriveJSON jsonOptions ''BorrowingStatus)
instance IsSqlEnum BorrowingStatus where
  data SqlEnum BorrowingStatus
  type SqlTypeName BorrowingStatus = "borrowing_status"


data Borrowing' a b c d = Borrowing
  { _book   :: a
  , _user   :: b
  , _date   :: c
  , _status :: d
  }
$(deriveJSON jsonOptions ''Borrowing')
$(makeAdaptorAndInstanceInferrable' ''Borrowing')
type Borrowing = Entity (Borrowing' Int64 Int64 UTCTime BorrowingStatus)
type BorrowingDetail = Entity (Borrowing' Book User UTCTime BorrowingStatus)
type BorrowingR = EntityR ( Borrowing' (Field SqlInt8)
                                       (Field SqlInt8)
                                       (Field SqlTimestamptz)
                                       (Field (SqlEnum BorrowingStatus))
                          )
type BorrowingW = EntityW ( Borrowing' (Field SqlInt8)
                                       (Field SqlInt8)
                                       (Field SqlTimestamptz)
                                       (Field (SqlEnum BorrowingStatus))
                          )
