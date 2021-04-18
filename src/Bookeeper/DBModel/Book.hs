module Bookeeper.DBModel.Book
  ( Book'(..)
  , Book
  , BookR
  , BookW
  , pBook

  , books
  ) where


import Protolude

import Opaleye
import Data.Aeson.TH
import Data.Profunctor.Product.TH

import Bookeeper.Util
import Bookeeper.DBModel.Entity


data Book' a b c = Book
  { sn     :: a
  , title  :: b
  , authur :: c
  }
$(deriveJSON jsonOptions ''Book')
$(makeAdaptorAndInstanceInferrable' ''Book')
type Book = Book' Text Text Text
type BookF = Book' (Field SqlText)
                   (Field SqlText)
                   (Field SqlText)
type BookR = EntityR BookF
type BookW = EntityW BookF

books :: Table BookW BookR
books = table "books" $ withEntity $ pBook Book
  { sn     = tableField "sn"
  , title  = tableField "title"
  , authur = tableField "authur"
  }
