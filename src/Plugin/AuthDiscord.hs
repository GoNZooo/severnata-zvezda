{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plugin.AuthDiscord where

import ClassyPrelude.Yesod
import Data.Aeson.Types (Result (..), parseMaybe, withObject)
import Yesod.Auth

type Form x site = Html -> MForm (HandlerFor site) (FormResult x, WidgetFor site ())

newtype FormUsername = FormUsername Text

class (YesodAuth site, FromJSON (RequestId site), (RenderMessage site Text)) => AuthDiscord site where
  type RequestId site
  type LoginUser site
  isLoginRequestApproved :: RequestId site -> HandlerFor site Bool
  getUserForLoginRequest :: RequestId site -> HandlerFor site (Maybe (LoginUser site))
  encodeUser :: LoginUser site -> Text

authDiscord :: forall site. (AuthDiscord site) => AuthPlugin site
authDiscord =
  let dispatch "POST" [] = authDiscordPost >>= sendResponse
      dispatch _ _ = notFound
      loginForm extra = do
        (usernameResult, usernameView) <- mreq textField (fieldSettings "Username") Nothing
        let loginWidget = do
              [whamlet|
                  #{extra}
                  <div>
                      ^{fvInput usernameView}
              |]
        pure (usernameResult, loginWidget)
      -- @TODO: add login route that does the waiting for response that we do via the user routes now.
      -- This can be done via the second argument to `dispatch` which is basically a split path.
      login authToMaster = do
        (formWidget, formEncodingType) <- generateFormPost loginForm
        [whamlet|
            <form method=post action=@{authToMaster $ PluginR "discord" []} enctype=#{formEncodingType}>
                <div id="discord-login">
                    ^{formWidget}
                <button .btn>Login via Discord
        |]
      fieldSettings :: Text -> FieldSettings site
      fieldSettings label =
        FieldSettings
          { fsLabel = SomeMessage label,
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Nothing,
            fsAttrs = []
          }
   in AuthPlugin "discord" dispatch login

authDiscordPost :: forall site. (AuthDiscord site) => AuthHandler site TypedContent
authDiscordPost = do
  let parseLoginRequestIdBody = withObject "loginRequestIdBody" $ \o -> do
        o .: "loginRequestId"
  maybeJsonValue <- parseCheckJsonBody
  case maybeJsonValue of
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
