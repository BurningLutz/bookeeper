module Bookeeper.DBModel.Borrowing
  ( Borrowing'(..)
  , Borrowing
  , BorrowingR
  , BorrowingW
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


data BorrowingStatus = Pending | Approved | Returned
  deriving stock (Show, Read)
$(deriveJSON jsonOptions ''BorrowingStatus)
instance IsSqlEnum BorrowingStatus where
  data SqlEnum BorrowingStatus
  type SqlTypeName BorrowingStatus = "borrowing_status"


data Borrowing' a b c d = Borrowing
  { _bookId :: a
  , _userId :: b
  , _date   :: c
  , _status :: d
  }
$(deriveJSON jsonOptions ''Borrowing')
$(makeAdaptorAndInstanceInferrable' ''Borrowing')
type Borrowing = Entity (Borrowing' Int64 Int64 UTCTime BorrowingStatus)
type BorrowingR = EntityR ( Borrowing' (Field SqlInt8)
                                       (Field SqlInt8)
                                       (Field SqlTimestamptz)
                                       (Field BorrowingStatus)
                          )
type BorrowingW = EntityW ( Borrowing' ()
                                       ()
                                       (Field SqlTimestamptz)
                                       (Field BorrowingStatus)
                          )
