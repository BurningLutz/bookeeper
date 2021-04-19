module Bookeeper.Model
  ( module Bookeeper.DBModel.Entity
  , module Bookeeper.DBModel.Admin
  , module Bookeeper.DBModel.User
  , module Bookeeper.DBModel.Book
  , module Bookeeper.DBModel.Borrowing
  , Env(..)
  , ClaimAdmin(..)
  , ClaimUser(..)
  , WithAccessToken(..)

  , AdminLogin(..)
  , UserLogin(..)
  , AddUser(..)
  , SetUser(..)
  , AddBook(..)
  , SetBook(..)
  , AddBorrowing(..)
  , SetBorrowing(..)

  , access_token
  , payload
  , nickname
  , password
  , age
  , isVip
  , id
  , createdAt
  , updatedAt
  , value
  , sn
  , title
  , author
  , book
  , user
  , date
  , status
  , bookId
  )
  where


import Protolude

import Data.Pool
import Data.Aeson.TH
import Control.Lens.TH
import Servant.Auth.Server
import Database.PostgreSQL.Simple

import Bookeeper.Util
import Bookeeper.DBModel.Entity
import Bookeeper.DBModel.Admin
import Bookeeper.DBModel.User
import Bookeeper.DBModel.Book
import Bookeeper.DBModel.Borrowing


$(makeFieldsNoPrefix ''Entity')
$(makeFieldsNoPrefix ''Admin')
$(makeFieldsNoPrefix ''User')
$(makeFieldsNoPrefix ''Book')
$(makeFieldsNoPrefix ''Borrowing')


data Env = Env
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  , pool           :: Pool Connection
  }


newtype ClaimAdmin = ClaimAdmin
  { _nickname :: Text
  }
$(makeFieldsNoPrefix ''ClaimAdmin)
-- its important to include the tag name here to differ from a ClaimUser
$(deriveJSON (jsonOptions { tagSingleConstructors = True }) ''ClaimAdmin)
deriving instance FromJWT ClaimAdmin
deriving instance ToJWT   ClaimAdmin


data ClaimUser = ClaimUser
  { _id       :: Int64
  , _nickname :: Text
  , _isVip    :: Bool
  }
$(makeFieldsNoPrefix ''ClaimUser)
-- its important to include the tag name here to differ from a ClaimAdmin
$(deriveJSON (jsonOptions { tagSingleConstructors = True }) ''ClaimUser)
deriving instance FromJWT ClaimUser
deriving instance ToJWT   ClaimUser


data WithAccessToken a = WithAccessToken
  { _access_token :: Text
  , _payload :: a
  }
$(makeFieldsNoPrefix ''WithAccessToken)
$(deriveJSON jsonOptions ''WithAccessToken)


data AdminLogin = AdminLogin
  { _nickname :: Text
  , _password :: Text
  }
$(makeFieldsNoPrefix ''AdminLogin)
$(deriveJSON jsonOptions ''AdminLogin)

newtype UserLogin = UserLogin
  { _nickname :: Text
  }
$(makeFieldsNoPrefix ''UserLogin)
$(deriveJSON jsonOptions ''UserLogin)

data AddUser = AddUser
  { _nickname :: Text
  , _isVip    :: Bool
  , _age      :: Maybe Int32
  }
$(makeFieldsNoPrefix ''AddUser)
$(deriveJSON jsonOptions ''AddUser)

data SetUser = SetUser
  { _isVip :: Bool
  , _age   :: Maybe Int32
  }
$(makeFieldsNoPrefix ''SetUser)
$(deriveJSON jsonOptions ''SetUser)

data AddBook = AddBook
  { _sn     :: Text
  , _title  :: Text
  , _author :: Text
  }
$(makeFieldsNoPrefix ''AddBook)
$(deriveJSON jsonOptions ''AddBook)

data SetBook = SetBook
  { _title  :: Text
  , _author :: Text
  }
$(makeFieldsNoPrefix ''SetBook)
$(deriveJSON jsonOptions ''SetBook)

newtype AddBorrowing = AddBorrowing
  { _bookId :: Int64
  }
$(makeFieldsNoPrefix ''AddBorrowing)
$(deriveJSON jsonOptions ''AddBorrowing)

newtype SetBorrowing = SetBorrowing
  { _status :: BorrowingStatus
  }
$(makeFieldsNoPrefix ''SetBorrowing)
$(deriveJSON jsonOptions ''SetBorrowing)
