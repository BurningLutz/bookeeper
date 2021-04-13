module Bookeeper.APIModel
  ( AddUser(..)
  , AdminLogin(..)
  ) where


import Protolude

import Data.Aeson.TH


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
