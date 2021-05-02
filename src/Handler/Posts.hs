{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Posts where

import qualified Data.List as List
import Data.Time.Calendar (showGregorian)
import Handler.Helpers
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data BlogPostForm = BlogPostForm
  { title :: Text,
    body :: Textarea
  }

data DeleteForm = DeleteForm

deleteFormRender :: Form DeleteForm
deleteFormRender = renderBootstrap3 BootstrapBasicForm $ pure DeleteForm

getPostsR :: Handler Html
getPostsR = do
  maybeUserId <- maybeAuthId
  (formWidget, formEncodingType) <- generateFormPost blogPostForm
  (deleteFormWidget, deleteFormEncodingType) <- generateFormPost deleteFormRender
  allPostEntities <- runDB getAllPosts
  let posts =
        List.map
          ( \p ->
              let postValue = entityVal p
                  postKey = entityKey p
                  deletePostForm = deleteForm "Delete" (PostR postKey) deleteFormWidget deleteFormEncodingType
               in (postKey, postValue, Just (blogPostUserId postValue) == maybeUserId, deletePostForm)
          )
          allPostEntities
      loggedIn = isJust maybeUserId

  defaultLayout $ do
    setTitle "Posts"
    $(widgetFile "posts")

postPostsR :: Handler Html
postPostsR = do
  maybeUser <- maybeAuthId
  ((result, _formWidget), _ormEncodingType) <- runFormPost blogPostForm
  case result of
    FormSuccess BlogPostForm {title, body} -> do
      case maybeUser of
        Just userId -> do
          now <- liftIO getCurrentTime
          let blogPostTitle = title
              blogPostBody = unTextarea body
              blogPostUserId = userId
              blogPostCreated = now
              blogPostUpdated = now
          insertResult <-
            runDB $
              insertPost $
                BlogPost
                  { blogPostTitle,
                    blogPostBody,
                    blogPostUserId,
                    blogPostCreated,
                    blogPostUpdated
                  }
          case insertResult of
            Just userId' -> defaultLayout $ do
              redirect $ PostR userId'
            Nothing -> invalidArgs []
        Nothing -> invalidArgs []
    _ -> invalidArgs []

blogPostForm :: Form BlogPostForm
blogPostForm =
  let fieldSettings label =
        FieldSettings
          { fsLabel = SomeMessage label,
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Nothing,
            fsAttrs = [("class", "form-control"), ("placeholder", label)]
          }
   in renderBootstrap3 BootstrapBasicForm $
        BlogPostForm
          <$> areq textField (fieldSettings "Title") Nothing
          <*> areq textareaField (fieldSettings "Body") Nothing

getAllPosts :: DB [Entity BlogPost]
getAllPosts = selectList [] [Desc BlogPostId]

insertPost :: BlogPost -> DB (Maybe (Key BlogPost))
insertPost = insertUnique
