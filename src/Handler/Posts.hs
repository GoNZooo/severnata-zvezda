{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Posts where

import qualified Data.List as List
import Handler.Helpers
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data PostForm = PostForm
  { title :: Text,
    body :: Textarea
  }

data DeleteForm = DeleteForm

deleteFormRender :: Form DeleteForm
deleteFormRender = renderBootstrap3 BootstrapBasicForm $ pure DeleteForm

getPostsR :: Handler Html
getPostsR = do
  maybeUserId <- maybeAuthId
  (formWidget, formEncodingType) <- generateFormPost postForm
  (deleteFormWidget, deleteFormEncodingType) <- generateFormPost deleteFormRender
  allPostEntities <- runDB getAllPosts
  let posts =
        List.map
          ( \p ->
              let postValue = entityVal p
                  postKey = entityKey p
                  deletePostForm = deleteForm "Delete" (PostR postKey) deleteFormWidget deleteFormEncodingType
               in (postKey, postValue, maybe False (== postUserId postValue) maybeUserId, deletePostForm)
          )
          allPostEntities

  defaultLayout $ do
    setTitle "Posts"
    $(widgetFile "posts")

postPostsR :: Handler Html
postPostsR = do
  maybeUser <- maybeAuthId
  ((result, _formWidget), _ormEncodingType) <- runFormPost postForm
  case result of
    FormSuccess (PostForm title' body') -> do
      case maybeUser of
        Just userId -> do
          insertResult <-
            runDB $
              insertPost $
                Post
                  { postTitle = title',
                    postBody = unTextarea body',
                    postUserId = userId
                  }
          case insertResult of
            Just userId' -> defaultLayout $ do
              redirect $ PostR userId'
            Nothing -> invalidArgs []
        Nothing -> invalidArgs []
    _ -> invalidArgs []

postForm :: Form PostForm
postForm =
  let fieldSettings label =
        FieldSettings
          { fsLabel = SomeMessage label,
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Nothing,
            fsAttrs = [("class", "form-control"), ("placeholder", label)]
          }
   in renderBootstrap3 BootstrapBasicForm $
        PostForm <$> areq textField (fieldSettings "Title") Nothing <*> areq textareaField (fieldSettings "Body") Nothing

getAllPosts :: DB [Entity Post]
getAllPosts = selectList [] [Asc PostId]

insertPost :: Post -> DB (Maybe (Key Post))
insertPost post = insertUnique post
