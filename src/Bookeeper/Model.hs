module Bookeeper.Model
  ( module Bookeeper.DBModel.Entity
  , module Bookeeper.DBModel.Admin
  , module Bookeeper.DBModel.User
  , module Bookeeper.DBModel.Book
  , module Bookeeper.DBModel.Borrowing
  , AppConfig(..)
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
import Data.Swagger (ToSchema)
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


data AppConfig = AppConfig
  { jwtSecret :: Text
  , port :: Int

  , dbHost :: Maybe Text
  , dbName :: Maybe Text
  , dbUser :: Maybe Text
  , dbPoolMaxConns :: Int
  }
$(deriveJSON jsonOptions ''AppConfig)


data Env = Env
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  , pool           :: Pool Connection
  }


newtype ClaimAdmin = ClaimAdmin
  { _nickname :: Text
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''ClaimAdmin)
-- its important to include the tag name here to differ from a ClaimUser
$(deriveJSON (jsonOptions { tagSingleConstructors = True }) ''ClaimAdmin)
deriving instance FromJWT ClaimAdmin
deriving instance ToJWT   ClaimAdmin
instance ToSchema ClaimAdmin


data ClaimUser = ClaimUser
  { _id       :: Int64
  , _nickname :: Text
  , _isVip    :: Bool
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''ClaimUser)
-- its important to include the tag name here to differ from a ClaimAdmin
$(deriveJSON (jsonOptions { tagSingleConstructors = True }) ''ClaimUser)
deriving instance FromJWT ClaimUser
deriving instance ToJWT   ClaimUser
instance ToSchema ClaimUser


data WithAccessToken a = WithAccessToken
  { _access_token :: Text
  , _payload :: a
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''WithAccessToken)
$(deriveJSON jsonOptions ''WithAccessToken)
instance ToSchema a => ToSchema (WithAccessToken a)


data AdminLogin = AdminLogin
  { _nickname :: Text
  , _password :: Text
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''AdminLogin)
$(deriveJSON jsonOptions ''AdminLogin)
instance ToSchema AdminLogin

newtype UserLogin = UserLogin
  { _nickname :: Text
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''UserLogin)
$(deriveJSON jsonOptions ''UserLogin)
instance ToSchema UserLogin

data AddUser = AddUser
  { _nickname :: Text
  , _isVip    :: Bool
  , _age      :: Maybe Int32
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''AddUser)
$(deriveJSON jsonOptions ''AddUser)
instance ToSchema AddUser

data SetUser = SetUser
  { _isVip :: Bool
  , _age   :: Maybe Int32
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''SetUser)
$(deriveJSON jsonOptions ''SetUser)
instance ToSchema SetUser

data AddBook = AddBook
  { _sn     :: Text
  , _title  :: Text
  , _author :: Text
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''AddBook)
$(deriveJSON jsonOptions ''AddBook)
instance ToSchema AddBook

data SetBook = SetBook
  { _title  :: Text
  , _author :: Text
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''SetBook)
$(deriveJSON jsonOptions ''SetBook)
instance ToSchema SetBook

newtype AddBorrowing = AddBorrowing
  { _bookId :: Int64
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''AddBorrowing)
$(deriveJSON jsonOptions ''AddBorrowing)
instance ToSchema AddBorrowing

newtype SetBorrowing = SetBorrowing
  { _status :: BorrowingStatus
  }
  deriving stock (Generic)
$(makeFieldsNoPrefix ''SetBorrowing)
$(deriveJSON jsonOptions ''SetBorrowing)
instance ToSchema SetBorrowing
