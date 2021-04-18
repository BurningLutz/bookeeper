module Bookeeper.APIModel
  ( AdminLogin(..)
  , UserLogin(..)
  , AddUser(..)
  , UpdUser(..)
  ) where


import Protolude

import Data.Aeson.TH

import Bookeeper.Util


data AdminLogin = AdminLogin
  { nickname :: Text
  , password :: Text
  }
$(deriveJSON jsonOptions ''AdminLogin)

data UserLogin = UserLogin
  { nickname :: Text
  , password :: Text
  }
$(deriveJSON jsonOptions ''UserLogin)

data AddUser = AddUser
  { nickname :: Text
  , age      :: Int32
  , isVip    :: Bool
  }
$(deriveJSON jsonOptions ''AddUser)

data UpdUser = UpdUser
  { age   :: Int32
  , isVip :: Bool
  }
$(deriveJSON jsonOptions ''UpdUser)
