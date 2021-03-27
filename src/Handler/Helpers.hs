{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Helpers where

import Import

putForm :: Route site -> WidgetFor site () -> Enctype -> WidgetFor site ()
putForm = formWithMethod (fromString "PUT")

deleteForm :: Route site -> WidgetFor site () -> Enctype -> WidgetFor site ()
deleteForm = formWithMethod (fromString "DELETE")

formWithMethod :: Text -> Route site -> WidgetFor site () -> Enctype -> WidgetFor site ()
formWithMethod method' url formWidget formEncodingType = do
  [whamlet|
    <form .form-horizontal method=post action=@{url}?_method=#{method'} enctype=#{formEncodingType}>
        ^{formWidget}

        <button .btn.btn-primary type="submit">
            Submit
  |]
