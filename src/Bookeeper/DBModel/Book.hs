module Bookeeper.DBModel.Book
  ( Book'(..)
  , Book
  , BookR
  , BookW
  , pBook
  ) where


import Protolude

import Data.Swagger
import Data.Aeson.TH
import Data.Profunctor.Product.TH
import Opaleye

import Bookeeper.Util
import Bookeeper.DBModel.Entity


data Book' a b c = Book
  { _sn     :: a
  , _title  :: b
  , _author :: c
  }
  deriving stock (Generic)
$(deriveJSON jsonOptions ''Book')
$(makeAdaptorAndInstanceInferrable' ''Book')
type BookE = Book' Text Text Text
instance ToSchema BookE

type Book  = Entity BookE
type BookF = Book' (Field SqlText)
                   (Field SqlText)
                   (Field SqlText)
type BookR = EntityR BookF
type BookW = EntityW BookF
