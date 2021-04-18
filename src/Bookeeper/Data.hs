module Bookeeper.Data
  ( Env(..)
  , BorrowingStatus(..)
  , ClaimAdmin(..)
  , ClaimUser(..)
  , WithAccessToken(..)
  ) where


import Protolude

import Data.Pool
import Data.Aeson.TH
import Servant.Auth.Server
import Database.PostgreSQL.Simple

import Bookeeper.Util
import Bookeeper.Data.SqlEnum


data Env = Env
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  , pool           :: Pool Connection
  }


data BorrowingStatus = Pending | Approved | Returned
  deriving stock (Show, Read)
$(deriveJSON jsonOptions ''BorrowingStatus)
instance IsSqlEnum BorrowingStatus where
  data SqlEnum BorrowingStatus
  type SqlTypeName BorrowingStatus = "borrowing_status"


newtype ClaimAdmin = ClaimAdmin
  { nickname :: Text
  }
$(deriveJSON jsonOptions ''ClaimAdmin)
deriving instance FromJWT ClaimAdmin
deriving instance ToJWT   ClaimAdmin


data ClaimUser = ClaimUser
  { nickname :: Text
  , isVip    :: Bool
  }
$(deriveJSON jsonOptions ''ClaimUser)
deriving instance FromJWT ClaimUser
deriving instance ToJWT   ClaimUser


data WithAccessToken a = WithAccessToken
  { access_token :: Text
  , payload :: a
  }
$(deriveJSON jsonOptions ''WithAccessToken)
