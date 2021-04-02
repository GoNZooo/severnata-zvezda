{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.AddLogin where

import Database.Persist.Sql (BackendKey (..))
import Import

getAddLoginR :: LoginRequestId -> Handler ()
getAddLoginR loginRequestId' = do
  maybeUserId <- runDB $ getUserId loginRequestId'
  case maybeUserId of
    Just (UserKey (SqlBackendKey userId')) -> do
      redirectToPost $ AuthR $ PluginR "discordLogin" [tshow userId']
    -- setCreds True $ Creds {credsPlugin = "discordLogin", credsIdent = tshow userId', credsExtra = []}
    Nothing ->
      redirect PostsR

getUserId :: LoginRequestId -> DB (Maybe UserId)
getUserId loginRequestId' = do
  maybeLoginRequest <- get loginRequestId'
  pure $ case maybeLoginRequest of
    Just (LoginRequest _uuid' userId' _approved') -> Just userId'
    Nothing -> Nothing
