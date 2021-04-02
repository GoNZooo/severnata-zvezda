{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.CheckLogin where

import Import

postCheckLoginR :: LoginRequestId -> Handler Value
postCheckLoginR loginRequestId' = do
  loginVerified <- runDB $ checkLoginRequest loginRequestId'
  let status = if loginVerified then "ok" else "notVerified"
  pure $ object [("status", status)]

-- checkLoginRequest :: LoginRequestId -> DB Bool
-- checkLoginRequest loginRequestId' = do
--   maybeLoginRequest <- get loginRequestId'
--   pure $ case maybeLoginRequest of
--     Just (LoginRequest _ _ (Just _approvedTime)) -> True
--     _ -> False
