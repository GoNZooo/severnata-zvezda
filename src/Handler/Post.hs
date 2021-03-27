{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Post where

import Import
import qualified Text.Markdown as Markdown
import Yesod.Form.Bootstrap3

getPostR :: PostId -> Handler Html
getPostR postId = do
  maybePost <- runDB $ getPost postId
  case maybePost of
    Just (Entity _postId (Post title' body' _userId)) ->
      defaultLayout $ do
        setTitle $ toHtml title'
        let renderedMarkdown = Markdown.markdown def (fromStrict body')
        $(widgetFile "post")
    Nothing -> notFound

putPostR :: PostId -> Handler Html
putPostR postId = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Just userId' -> do
      ((result, _formWidget), _ormEncodingType) <- runFormPost postForm
      case result of
        FormSuccess (PostForm {title = title', body = body'}) -> do
          runDB $ modifyPost (Entity postId $ Post {postTitle = title', postBody = body', postUserId = userId'})
          redirect $ PostR postId
        _ -> invalidArgs []
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
        PostForm <$> areq textField (fieldSettings "Title") Nothing <*> areq textField (fieldSettings "Body") Nothing

getPost :: PostId -> DB (Maybe (Entity Post))
getPost = getEntity

modifyPost :: Entity Post -> DB ()
modifyPost post = replace (entityKey post) $ entityVal post
