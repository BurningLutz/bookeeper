module Bookeeper.Util
  ( Env(..)
  , AdminLevel(..)
  , BorrowingStatus(..)
  , ClaimAdmin(..)
  , WithAccessToken(..)

  , jsonOptions
  , writeOnlyTableField
  ) where


import Protolude
import Prelude (String)

import Opaleye
import Data.Aeson.TH
import Data.Profunctor
import Servant.Auth.Server


data Env = Env
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  }

data AdminLevel = Low | Mid | High
$(deriveJSON defaultOptions ''AdminLevel)

data BorrowingStatus = Pending | Approved | Returned
$(deriveJSON defaultOptions ''BorrowingStatus)

data ClaimAdmin = ClaimAdmin
  { nickname :: Text
  , level    :: AdminLevel
  }
$(deriveJSON defaultOptions ''ClaimAdmin)
deriving instance FromJWT ClaimAdmin
deriving instance ToJWT   ClaimAdmin

data WithAccessToken a = WithAccessToken
  { access_token :: Text
  , payload :: a
  }
$(deriveJSON defaultOptions ''WithAccessToken)


jsonOptions :: Options
jsonOptions = defaultOptions
  { fieldLabelModifier     = dropWhile (== '_')
  , constructorTagModifier = map toLower
  }

writeOnlyTableField :: String -> TableFields (Field a) ()
writeOnlyTableField = rmap (const ()) . requiredTableField
