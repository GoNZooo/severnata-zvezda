{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plugin.AuthDiscord where

import ClassyPrelude.Yesod
import Data.Aeson.Types (Result (..), parseMaybe, withObject)
import Yesod.Auth

class (YesodAuth site, FromJSON (RequestId site)) => AuthDiscord site where
  type RequestId site
  type LoginUser site
  isLoginRequestApproved :: RequestId site -> HandlerFor site Bool
  getUserForLoginRequest :: RequestId site -> HandlerFor site (Maybe (LoginUser site))
  encodeUser :: LoginUser site -> Text

authDiscord :: (AuthDiscord site) => AuthPlugin site
authDiscord =
  let dispatch "POST" [] = authDiscordPost >>= sendResponse
      dispatch _ _ = notFound
      login _authToMaster = do
        _request <- getRequest
        toWidget [hamlet|<h2>Hello|]
   in AuthPlugin "discord" dispatch login

authDiscordPost :: forall site. (AuthDiscord site) => AuthHandler site TypedContent
authDiscordPost = do
  let parseLoginRequestIdBody = withObject "loginRequestIdBody" $ \o -> do
        o .: "loginRequestId"
  maybeJsonContent :: Result Value <- parseCheckJsonBody
  case maybeJsonContent of
    Success jsonContent -> do
      case parseMaybe parseLoginRequestIdBody jsonContent of
        Just loginRequestId' -> do
          isApproved <- liftHandler $ isLoginRequestApproved loginRequestId'
          if isApproved
            then do
              maybeUser <- liftHandler $ getUserForLoginRequest loginRequestId'
              case maybeUser of
                Just user -> do
                  setCredsRedirect $ Creds "discord" (encodeUser @site user) []
                Nothing -> notFound
            else do
              notFound
        Nothing -> notFound
    _other -> do
      notFound
