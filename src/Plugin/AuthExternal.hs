{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plugin.AuthExternal where

import ClassyPrelude.Yesod
import Control.Monad.Trans.RWS.Lazy (RWST)
import Data.Aeson.Types (Result (..), parseMaybe, withObject)
import Settings (widgetFile)
import Text.Blaze (Markup)
import Text.Julius (ToJavascript)
import Yesod.Auth

type Form x site = Html -> MForm (HandlerFor site) (FormResult x, WidgetFor site ())

newtype FormUsername = FormUsername Text

class
  ( YesodAuth site,
    FromJSON (RequestId site),
    ToJavascript (RequestId site),
    (RenderMessage site Text)
  ) =>
  AuthExternal site
  where
  type RequestId site
  type LoginUser site
  isLoginRequestApproved :: RequestId site -> HandlerFor site Bool
  approveLoginRequest :: RequestId site -> HandlerFor site ()
  getUserForLoginRequest :: RequestId site -> HandlerFor site (Maybe (LoginUser site))
  encodeUser :: LoginUser site -> Text
  createLoginRequest :: LoginUser site -> HandlerFor site (Maybe (RequestId site))
  getUserByUsername :: Text -> HandlerFor site (Maybe (LoginUser site))

authExternal :: forall site. (AuthExternal site) => AuthPlugin site
authExternal =
  let dispatch "POST" ["approve"] = approveRequestHandler >>= sendResponse
      dispatch "POST" ["wait"] = waitApprovalHandler >>= sendResponse
      dispatch "POST" ["check"] = checkApprovalHandler >>= sendResponse
      dispatch _ _ = notFound
      -- @TODO: add login route that does the waiting for response that we do via the user routes now.
      -- This can be done via the second argument to `dispatch` which is basically a split path.
      login authToMaster = do
        (formWidget, formEncodingType) <- generateFormPost loginForm
        [whamlet|
            <form method=post action=@{authToMaster $ PluginR "discord" ["wait"]} enctype=#{formEncodingType}>
                <div id="discord-login">
                    ^{formWidget}
                <button .btn>Login via Discord
        |]
   in AuthPlugin "discord" dispatch login

waitApprovalHandler ::
  forall site.
  (AuthExternal site, RenderMessage site FormMessage) =>
  AuthHandler site Html
waitApprovalHandler = do
  formResult <- runInputPostResult (ireq textField "username")
  toParentRoute <- getRouteToParent
  case formResult of
    FormSuccess username' -> do
      maybeLoginUser <- liftHandler $ getUserByUsername username'
      case maybeLoginUser of
        Just loginUser -> do
          maybeLoginRequestId <- liftHandler $ createLoginRequest loginUser
          case maybeLoginRequestId of
            Just loginRequestIdToCheck -> do
              yesodSite <- getYesod
              let authPostRoute = toParentRoute $ PluginR "discord" ["check"]
                  successRedirectRoute = loginDest yesodSite
              liftHandler $
                defaultLayout $(widgetFile "new-user-wait")
            Nothing -> invalidArgs []
        Nothing -> notFound
    _ -> invalidArgs []

loginForm ::
  forall site.
  (RenderMessage site FormMessage) =>
  Markup ->
  RWST
    (Maybe (Env, FileEnv), site, [Lang])
    Enctype
    Ints
    (WidgetFor site)
    (FormResult Text, WidgetFor site ())
loginForm extra = do
  (usernameResult, usernameView) <- mreq textField (fieldSettings "Username") Nothing
  let loginWidget = do
        [whamlet|
            #{extra}
            <div>
                ^{fvInput usernameView}
        |]
  pure (usernameResult, loginWidget)

fieldSettings :: Text -> FieldSettings site
fieldSettings label =
  FieldSettings
    { fsLabel = SomeMessage label,
      fsTooltip = Nothing,
      fsId = Nothing,
      fsName = Just $ toLower label,
      fsAttrs = []
    }

approveRequestHandler :: forall site. (AuthExternal site) => AuthHandler site Value
approveRequestHandler = do
  let parseLoginRequestIdBody = withObject "loginRequestIdBody" $ \o -> do
        o .: "loginRequestId"
  maybeJsonValue <- parseCheckJsonBody
  case maybeJsonValue of
    Success jsonContent -> do
      case parseMaybe parseLoginRequestIdBody jsonContent of
        Just loginRequestId' -> do
          liftHandler $ approveLoginRequest loginRequestId'
          pure $ object ["approved" .= True]
        Nothing -> notFound
    _other -> do
      notFound

checkApprovalHandler :: forall site. (AuthExternal site) => AuthHandler site TypedContent
checkApprovalHandler = do
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
