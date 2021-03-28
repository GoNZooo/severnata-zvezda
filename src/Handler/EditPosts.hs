{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditPosts where

import Handler.Helpers
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data PostForm = PostForm
  { title :: Text,
    body :: Textarea,
    userId :: UserId,
    id :: PostId
  }

getEditPostR :: PostId -> Handler Html
getEditPostR postId = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Just userId' -> do
      maybePostEntity <- runDB $ getPost postId
      let titleValue = maybe "" (postTitle . entityVal) maybePostEntity
          bodyValue = maybe "" (Textarea . postBody . entityVal) maybePostEntity
      (formWidget, formEncodingType) <- generateFormPost $ postForm userId' postId titleValue bodyValue
      let editForm = putForm "Submit" (PostR postId) formWidget formEncodingType
      case maybePostEntity of
        Just (Entity _postId post) -> defaultLayout $ do
          setTitle $ "Edit Post: " <> toHtml (postTitle post)
          $(widgetFile "edit-post")
        Nothing -> notFound
    Nothing -> notAuthenticated

postForm :: UserId -> PostId -> Text -> Textarea -> Form PostForm
postForm userId' postId title' body' =
  let fieldSettings label =
        FieldSettings
          { fsLabel = SomeMessage label,
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Nothing,
            fsAttrs = [("class", "form-control"), ("placeholder", label)]
          }
   in renderBootstrap3 BootstrapBasicForm $
        PostForm
          <$> areq textField (fieldSettings "Title") (Just title')
          <*> areq textareaField (fieldSettings "Body") (Just body')
          <*> areq hiddenField "" (Just userId')
          <*> areq hiddenField "" (Just postId)

getPost :: PostId -> DB (Maybe (Entity Post))
getPost = getEntity
