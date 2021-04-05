{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OrphanInstances where

import ClassyPrelude
import qualified Data.ByteString.Char8 as ByteString8
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Persist.Sql
  ( PersistField (..),
    PersistFieldSql (..),
    PersistValue (..),
    SqlType (..),
  )
import Text.Blaze (ToMarkup (..))
import Text.Julius (ToJavascript (..))

instance PersistField UUID where
  toPersistValue uuid = PersistDbSpecific . fromString . UUID.toString $ uuid
  fromPersistValue (PersistDbSpecific v) = case UUID.fromString $ ByteString8.unpack v of
    Just v' -> Right v'
    Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not `PersistDbSpecific`"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance ToJavascript UUID where
  toJavascript = toJavascript . UUID.toText

instance ToMarkup UUID where
  toMarkup = toMarkup . UUID.toText
