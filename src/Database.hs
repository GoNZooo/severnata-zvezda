{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Database where

import ClassyPrelude hiding (on)
import Data.Aeson (FromJSON)
import Data.UUID (UUID)
import Database.Esqueleto.Experimental
import qualified Database.Persist as Persist
import Model

data ChallengeApprovalPayload = ChallengeApprovalPayload
  { uuid :: UUID,
    username :: Text,
    discordUsername :: Text,
    token :: Token
  }
  deriving (Eq, Show, Generic, FromJSON)

appApproveLoginRequest :: (MonadUnliftIO m) => ChallengeApprovalPayload -> ReaderT SqlBackend m Bool
appApproveLoginRequest
  ChallengeApprovalPayload
    { uuid = uuid',
      username = username',
      discordUsername = discordUsername',
      token = Token {tokenService = service', tokenKey = key'}
    } = do
    maybeToken <- Persist.getBy $ UniqueService service'
    case maybeToken of
      Just (Entity _tokenId (Token existingTokenKey existingTokenService))
        | existingTokenKey == key' && existingTokenService == service' -> do
          currentTime <- liftIO getCurrentTime

          matches <- select $ do
            (users :& loginRequests) <-
              from $
                Table @User `InnerJoin` Table @LoginRequest
                  `on` ( \(users :& loginRequest) ->
                           users ^. UserId ==. loginRequest ^. LoginRequestUserId
                       )
            where_
              ( users ^. UserUsername ==. val username'
                  &&. loginRequests ^. LoginRequestUuid ==. val uuid'
              )
            pure (users, loginRequests)

          case matches of
            [ ( Entity _userId' User {userDiscordUsername = Just existingDiscordUsername},
                Entity loginRequestId' _loginRequest
                )
              ]
                | existingDiscordUsername == discordUsername' -> do
                  Persist.update loginRequestId' [LoginRequestApproved Persist.=. Just currentTime]
                  pure True
            [ ( Entity userId' User {userDiscordUsername = Nothing},
                Entity loginRequestId' _loginRequest
                )
              ] -> do
                Persist.update loginRequestId' [LoginRequestApproved Persist.=. Just currentTime]
                Persist.update userId' [UserDiscordUsername Persist.=. Just discordUsername']
                pure True
            [] -> error "No results"
            _ -> error "Too many results"
      Just _token -> error "Invalid token"
      Nothing -> error "Invalid token"
