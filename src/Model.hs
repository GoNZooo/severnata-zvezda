{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model where

import ClassyPrelude.Yesod
import Data.UUID (UUID)
import Database.Persist.Quasi
import Database.Persist.Sql (unSqlBackendKey)
import OrphanInstances ()
import Text.Blaze (ToMarkup (..))
import Text.Julius (ToJavascript (..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

instance ToJavascript LoginRequestId where
  toJavascript = toJavascript . Number . fromIntegral . unSqlBackendKey . unLoginRequestKey

instance ToMarkup LoginRequestId where
  toMarkup = toMarkup @Int64 . fromIntegral . unSqlBackendKey . unLoginRequestKey
