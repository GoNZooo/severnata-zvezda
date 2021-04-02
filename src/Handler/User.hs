{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User where

import qualified Data.UUID.V4 as UUIDv4
import Handler.Helpers
import Import
import Yesod.Form.Bootstrap3

getUserR :: Handler Html
getUserR = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Just _ ->
      defaultLayout $ redirect PostsR
    Nothing -> do
      (userFormWidget, userFormEncodingType) <- generateFormPost userForm
      let newUserForm = postForm "Create" UserR userFormWidget userFormEncodingType
      defaultLayout $ do
        setTitle "New User"
        $(widgetFile "new-user")

postUserR :: Handler Html
postUserR = do
  ((result, _), _) <- runFormPost userForm
  case result of
    FormSuccess (UserForm username' discordUsername') -> do
      maybeLoginRequestId <- runDB $ do
        userId' <- newOrExistingUser username' discordUsername'
        newLoginRequest userId'
      case maybeLoginRequestId of
        Just loginRequestIdToCheck -> defaultLayout $ do
          let authPostRoute = AuthR $ PluginR "discord" []
          $(widgetFile "new-user-wait")
        Nothing -> error "Couldn't insert user"
    _ -> invalidArgs []

newOrExistingUser :: Text -> Text -> DB UserId
newOrExistingUser username' discordUsername' = do
  maybeExisting <- getBy $ UniqueUsername username'
  case maybeExisting of
    Just (Entity userId' _user) -> pure userId'
    Nothing -> unsafeFromJust <$> insertUnique (User username' discordUsername')

newLoginRequest :: UserId -> DB (Maybe LoginRequestId)
newLoginRequest userId' = do
  newUuid <- liftIO UUIDv4.nextRandom
  insertUnique $ LoginRequest newUuid userId' Nothing

data UserForm = UserForm
  { username :: Text,
    discordUsername :: Text
  }

userForm :: Form UserForm
userForm =
  let fieldSettings label =
        FieldSettings
          { fsLabel = SomeMessage label,
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Nothing,
            fsAttrs = [("class", "form-control"), ("placeholder", label)]
          }
   in renderBootstrap3 BootstrapBasicForm $
        UserForm
          <$> areq textField (fieldSettings "Username") Nothing
          <*> areq textField (fieldSettings "Discord Username") Nothing

unsafeFromJust :: Maybe a -> a
unsafeFromJust (Just a) = a
unsafeFromJust Nothing = error "Tried to unpack `Nothing`"
