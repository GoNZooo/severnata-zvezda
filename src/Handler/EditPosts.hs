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
      (formWidget, formEncodingType) <- generateFormPost $ postForm userId' postId
      let editForm = putForm "Submit" (PostR postId) formWidget formEncodingType
      maybePostEntity <- runDB $ getPost postId
      case maybePostEntity of
        Just (Entity _postId post) -> defaultLayout $ do
          setTitle $ "Edit Post: " <> toHtml (postTitle post)
          $(widgetFile "edit-post")
        Nothing -> notFound
    Nothing -> notAuthenticated

postForm :: UserId -> PostId -> Form PostForm
postForm userId' postId =
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
          <$> areq textField (fieldSettings "Title") Nothing
          <*> areq textareaField (fieldSettings "Body") Nothing
          <*> areq hiddenField "" (Just userId')
          <*> areq hiddenField "" (Just postId)

getPost :: PostId -> DB (Maybe (Entity Post))
getPost = getEntity
