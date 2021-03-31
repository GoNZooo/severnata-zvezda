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
    id :: BlogPostId
  }

getEditPostR :: BlogPostId -> Handler Html
getEditPostR postId = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Just userId' -> do
      maybePostEntity <- runDB $ getPost postId
      let titleValue = maybe "" (blogPostTitle . entityVal) maybePostEntity
          bodyValue = maybe "" (Textarea . blogPostBody . entityVal) maybePostEntity
      (formWidget, formEncodingType) <- generateFormPost $ postForm userId' postId titleValue bodyValue
      let editForm = putForm "Submit" (PostR postId) formWidget formEncodingType
      case maybePostEntity of
        Just (Entity _postId post) -> defaultLayout $ do
          setTitle $ "Edit Post: " <> toHtml (blogPostTitle post)
          $(widgetFile "edit-post")
        Nothing -> notFound
    Nothing -> notAuthenticated

data InputType = TitleInput | BodyInput deriving (Eq)

postForm :: UserId -> BlogPostId -> Text -> Textarea -> Form PostForm
postForm userId' postId title' body' =
  let fieldSettings inputType =
        let (elementId, label) = case inputType of
              TitleInput -> ("post-title-input", "Title")
              BodyInput -> ("post-body-input", "Body")
         in FieldSettings
              { fsLabel = SomeMessage label,
                fsTooltip = Nothing,
                fsId = Just elementId,
                fsName = Nothing,
                fsAttrs = [("class", "form-control"), ("placeholder", label)]
              }
   in renderBootstrap3 BootstrapBasicForm $
        PostForm
          <$> areq textField (fieldSettings TitleInput) (Just title')
          <*> areq textareaField (fieldSettings BodyInput) (Just body')
          <*> areq hiddenField "" (Just userId')
          <*> areq hiddenField "" (Just postId)

getPost :: BlogPostId -> DB (Maybe (Entity BlogPost))
getPost = getEntity
