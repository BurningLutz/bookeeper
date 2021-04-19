module Bookeeper.Util
  ( jsonOptions
  , writeOnlyTableField
  ) where


import Protolude
import Prelude (String)

import Data.Aeson.TH
import Data.Profunctor
import Opaleye


jsonOptions :: Options
jsonOptions = defaultOptions
  { fieldLabelModifier     = dropWhile (== '_')
  , constructorTagModifier = map toLower
  }

writeOnlyTableField :: String -> TableFields (Field a) ()
writeOnlyTableField = rmap (const ()) . requiredTableField
