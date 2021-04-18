module Bookeeper.Data.SqlEnum
  ( IsSqlEnum(..)
  ) where


import Protolude
import Prelude (String)

import Opaleye
import Opaleye.Experimental.Enum
import Opaleye.Internal.Inferrable (Inferrable(..))
import Data.Profunctor.Product.Default


class (Show hs, Read hs, KnownSymbol (SqlTypeName hs)) => IsSqlEnum hs where
  data SqlEnum hs
  type SqlTypeName hs :: Symbol

  toSqlType :: hs -> String
  toSqlType = map toLower . show

  fromSqlType :: String -> Maybe hs
  fromSqlType (x:xs) = readMaybe $ toUpper x:xs
  fromSqlType "" = Nothing

enumMapper :: forall hs
            . ( IsSqlEnum hs
              , KnownSymbol (SqlTypeName hs)
              )
           => ( FromField (SqlEnum hs) hs
              , ToFields hs (Column (SqlEnum hs))
              )
enumMapper = fromFieldToFieldsEnum (symbolVal $ Proxy @(SqlTypeName hs)) fromSqlType toSqlType

instance IsSqlEnum hs => DefaultFromField (SqlEnum hs) hs where
  defaultFromField = fst enumMapper

instance IsSqlEnum hs => Default (Inferrable FromFields) (Column (SqlEnum hs)) hs where
  def = Inferrable def

instance IsSqlEnum hs => Default ToFields hs (Column (SqlEnum hs)) where
  def = snd enumMapper
