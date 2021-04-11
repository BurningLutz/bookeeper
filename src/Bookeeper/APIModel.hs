module Bookeeper.APIModel
  ( User(..)
  , AddUser(..)
  , AdminLevel(..)
  , Admin(..)
  , ClaimAdmin(..)
  , Book(..)
  , BorrowingStatus(..)
  , Borrowing(..)
  , Env(..)
  , WithAccessToken(..)
  , AdminLogin(..)
  ) where


import Protolude

import Data.Time
import Data.Aeson.TH
import Servant.Auth.Server


data Env = Env
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  }

data User = User
  { id       :: Word64
  , nickname :: Text
  , age      :: Word8
  , isVip    :: Bool
  }
$(deriveJSON defaultOptions ''User)

data AdminLevel = Low | Mid | High
$(deriveJSON defaultOptions ''AdminLevel)

data Admin = Admin
  { nickname :: Text
  , level    :: AdminLevel
  }
$(deriveJSON defaultOptions ''Admin)

data ClaimAdmin = ClaimAdmin
  { nickname :: Text
  , level    :: AdminLevel
  }
$(deriveJSON defaultOptions ''ClaimAdmin)
deriving instance FromJWT ClaimAdmin
deriving instance ToJWT   ClaimAdmin

data Book = Book
  { sn     :: Text
  , title  :: Text
  , authur :: Text
  }
$(deriveJSON defaultOptions ''Book)

data BorrowingStatus = Pending | Approved | Returned
$(deriveJSON defaultOptions ''BorrowingStatus)

data Borrowing = Borrowing
  { book   :: Book
  , user   :: User
  , date   :: UTCTime
  , status :: BorrowingStatus
  }
$(deriveJSON defaultOptions ''Borrowing)

data WithAccessToken a = WithAccessToken
  { access_token :: Text
  , payload :: a
  }
$(deriveJSON defaultOptions ''WithAccessToken)

data AddUser = AddUser
  { nickname :: Text
  , age      :: Word8
  , isVip    :: Bool
  }
$(deriveJSON defaultOptions ''AddUser)

data AdminLogin = AdminLogin
  { nickname :: Text
  , password :: Text
  }
$(deriveJSON defaultOptions ''AdminLogin)
