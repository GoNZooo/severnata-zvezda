{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Post where

import Import

--import Text.Julius (RawJS (..))
--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getPostR :: PostId -> Handler Html
getPostR postId = do
  --  (formWidget, formEnctype) <- generateFormPost sampleForm
  --  let handlerName = "getPostR" :: Text
  --  allComments <- runDB getAllComments
  maybePost <- runDB $ getPost postId
  case maybePost of
    Just (Entity _postId (Post title body _userId)) ->
      defaultLayout $ do
        setTitle $ toHtml title
        $(widgetFile "post")
    Nothing -> notFound

--
--postHomeR :: Handler Html
--postHomeR = do
--  ((result, formWidget), formEnctype) <- runFormPost sampleForm
--  let handlerName = "postHomeR" :: Text
--      submission = case result of
--        FormSuccess res -> Just res
--        _ -> Nothing
--  allComments <- runDB $ getAllComments
--
--  defaultLayout $ do
--    let (commentFormId, commentTextareaId, commentListId) = commentIds
--    aDomId <- newIdent
--    setTitle "Welcome To Yesod!"
--    $(widgetFile "homepage")
--
--sampleForm :: Form FileForm
--sampleForm =
--  renderBootstrap3 BootstrapBasicForm $
--    FileForm
--      <$> fileAFormReq "Choose a file"
--      <*> areq textField textSettings Nothing
--  where
--    -- Add attributes like the placeholder and CSS classes.
--    textSettings =
--      FieldSettings
--        { fsLabel = "What's on the file?",
--          fsTooltip = Nothing,
--          fsId = Nothing,
--          fsName = Nothing,
--          fsAttrs =
--            [ ("class", "form-control"),
--              ("placeholder", "File description")
--            ]
--        }

--commentIds :: (Text, Text, Text)
--commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
--

getPost :: PostId -> DB (Maybe (Entity Post))
getPost = getEntity
