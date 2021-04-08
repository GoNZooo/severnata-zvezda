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
import Data.Aeson.Types (Result (..), parseEither, parseMaybe, withObject)
import Data.UUID (UUID)
import Settings (widgetFile)
import Text.Blaze (Markup, ToMarkup)
import Text.Julius (ToJavascript)
import Yesod.Auth

type Form x site = Html -> MForm (HandlerFor site) (FormResult x, WidgetFor site ())

newtype FormUsername = FormUsername Text

newtype ApprovalToken = ApprovalToken UUID

class
  ( YesodAuth site,
    FromJSON (RequestId site),
    FromJSON (ChallengePayload site),
    ToJavascript (RequestId site),
    ToMarkup (RequestId site),
    (RenderMessage site Text)
  ) =>
  AuthExternal site
  where
  type RequestId site
  type ChallengePayload site
  type LoginUser site

  -- | Used to approve a login request via the incoming challenge payload. The challenge should
  -- contain everything needed to successfully verify that the service approving the request is able
  -- to do so as well as the challenge being the one that corresponds to the login request being
  -- approved.
  approveLoginRequest :: ChallengePayload site -> HandlerFor site Bool

  -- | Used to consume the login request such that it cannot be used for logging in again, meaning
  -- we cannot replay login requests.
  markLoginRequestAsFollowed :: RequestId site -> HandlerFor site ()

  -- | Used to check an already existing login request for approval. This is called to observe the
  -- status of the login request, not to change it.
  isLoginRequestApproved :: RequestId site -> HandlerFor site Bool

  getUserForLoginRequest :: RequestId site -> HandlerFor site (Maybe (LoginUser site))

  -- | Used to encode the user data for placing in the user's session.
  encodeUser :: LoginUser site -> Text

  createLoginRequest :: LoginUser site -> HandlerFor site (Maybe (RequestId site))

  getUserByUsername :: Text -> HandlerFor site (Maybe (LoginUser site))

pluginName :: Text
pluginName = "external-approval"

authExternal :: forall site. (AuthExternal site) => AuthPlugin site
authExternal =
  let dispatch "POST" ["approve"] = approveRequestHandler >>= sendResponse
      dispatch "POST" ["wait"] = waitApprovalHandler >>= sendResponse
      dispatch "POST" ["check"] = checkApprovalHandler >>= sendResponse
      dispatch _ _ = notFound

      login authToMaster = do
        let routeToWaitPage = authToMaster $ PluginR pluginName ["wait"]
        (formWidget, formEncodingType) <- generateFormPost loginForm
        [whamlet|
            <form method=post action=@{routeToWaitPage} enctype=#{formEncodingType}>
                <div id="external-approval-login-form-div">
                    ^{formWidget}
                <button class="btn" id="external-approval-login-button">Login with external approval
        |]
   in AuthPlugin pluginName dispatch login

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
              let authPostRoute = toParentRoute $ PluginR pluginName ["check"]
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
  maybeJsonValue <- parseCheckJsonBody
  case maybeJsonValue of
    Success jsonContent -> do
      case parseEither parseJSON jsonContent of
        Right challengePayload -> do
          isApproved <- liftHandler $ approveLoginRequest challengePayload
          pure $ object ["approved" .= isApproved]
        Left _error -> do
          invalidArgs []
    _other -> do
      invalidArgs []

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
                  liftHandler $ markLoginRequestAsFollowed loginRequestId'
                  setCredsRedirect $ Creds pluginName (encodeUser @site user) []
                Nothing -> notFound
            else do
              notFound
        Nothing -> notFound
    _other -> do
      notFound
