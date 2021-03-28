{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Helpers where

import Import

putForm :: Text -> Route site -> WidgetFor site () -> Enctype -> WidgetFor site ()
putForm = formWithMethod (fromString "PUT")

deleteForm :: Text -> Route site -> WidgetFor site () -> Enctype -> WidgetFor site ()
deleteForm = formWithMethod (fromString "DELETE")

formWithMethod :: Text -> Text -> Route site -> WidgetFor site () -> Enctype -> WidgetFor site ()
formWithMethod method' submitText url formWidget formEncodingType = do
  [whamlet|
    <form .form-horizontal method=post action=@{url}?_method=#{method'} enctype=#{formEncodingType}>
        ^{formWidget}

        <button .btn.btn-primary type="submit">
            #{submitText}
  |]
