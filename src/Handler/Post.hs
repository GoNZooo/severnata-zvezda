{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Post where

import Import
import qualified Text.Markdown as Markdown
import Yesod.Form.Bootstrap3

getPostR :: BlogPostId -> Handler Html
getPostR postId = do
  maybePost <- runDB $ getPost postId
  maybeUserId <- maybeAuthId
  case maybePost of
    Just (Entity _postId (BlogPost title' body' userId)) ->
      defaultLayout $ do
        setTitle $ toHtml title'
        let renderedMarkdown = Markdown.markdown def $ fromStrict body'
            ownsPost = Just userId == maybeUserId
            editLink = [whamlet|<a href=@{EditPostR postId}>Edit|]
        $(widgetFile "post")
    Nothing -> notFound

putPostR :: BlogPostId -> Handler Html
putPostR postId = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Just userId' -> do
      ((result, _formWidget), _ormEncodingType) <- runFormPost postForm
      case result of
        FormSuccess PostForm {title = title', body = body'} -> do
          runDB $
            modifyPost
              ( Entity postId $
                  BlogPost
                    { blogPostTitle = title',
                      blogPostBody = body',
                      blogPostUserId = userId'
                    }
              )
          redirect $ PostR postId
        _ -> invalidArgs []
    Nothing ->
      notAuthenticated

deletePostR :: BlogPostId -> Handler Html
deletePostR postId = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Just _userId' -> do
      runDB $ deletePost postId
      redirect $ PostsR
    Nothing ->
      notAuthenticated

data PostForm = PostForm
  { title :: Text,
    body :: Text
  }

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
        PostForm
          <$> areq textField (fieldSettings "Title") Nothing
          <*> areq textField (fieldSettings "Body") Nothing

getPost :: BlogPostId -> DB (Maybe (Entity BlogPost))
getPost = getEntity

modifyPost :: Entity BlogPost -> DB ()
modifyPost post = replace (entityKey post) $ entityVal post

deletePost :: Key BlogPost -> DB ()
deletePost = delete
