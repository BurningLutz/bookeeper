module Bookeeper.DBModel.Borrowing
  ( Borrowing'(..)
  , Borrowing
  , BorrowingR
  , BorrowingW
  , pBorrowing

  , borrowings
  ) where


import Protolude

import Opaleye
import Data.Time
import Data.Aeson.TH
import Data.Profunctor.Product.TH

import Bookeeper.Data
import Bookeeper.Util
import Bookeeper.DBModel.Entity


data Borrowing' a b c d = Borrowing
  { bookId :: a
  , userId :: b
  , date   :: c
  , status :: d
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

borrowings :: Table BorrowingW BorrowingR
borrowings = table "borrowings" $ withEntity $ pBorrowing Borrowing
  { bookId = readOnlyTableField "book_id"
  , userId = readOnlyTableField "user_id"
  , date   = tableField "date"
  , status = tableField "status"
  }
