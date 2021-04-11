module Bookeeper.Util
  ( jsonOptions
  ) where


import Protolude

import Data.Aeson


jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier     = dropWhile (== '_')
                             , constructorTagModifier = map toLower
                             }
