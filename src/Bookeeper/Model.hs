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
  , UpdUser(..)

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
  , bookId
  , userId
  , date
  , status
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
$(deriveJSON jsonOptions ''ClaimAdmin)
deriving instance FromJWT ClaimAdmin
deriving instance ToJWT   ClaimAdmin


data ClaimUser = ClaimUser
  { _nickname :: Text
  , _isVip    :: Bool
  }
$(makeFieldsNoPrefix ''ClaimUser)
$(deriveJSON jsonOptions ''ClaimUser)
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

data UpdUser = UpdUser
  { _isVip :: Bool
  , _age   :: Maybe Int32
  }
$(makeFieldsNoPrefix ''UpdUser)
$(deriveJSON jsonOptions ''UpdUser)
