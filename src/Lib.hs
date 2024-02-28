module Lib (
    hello,
    application,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Foundation
import Foundation.Collection (forM_)
import Network.HTTP.Types (HeaderName, status200)
import Network.Wai
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx

hello :: Html
hello = H.docTypeHtml $ do
    H.head $ do
        H.title "Hello World"
        htmxScriptTag
    H.body $ do
        H.h1 "Hello World"

htmxScriptTag :: Html
htmxScriptTag =
    script ""
        ! src "https://unpkg.com/htmx.org@1.9.10"
        ! customAttribute "integrity" "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC"
        ! customAttribute "crossorigin" "anonymous"

contentTypeHtml :: (IsString s) => (HeaderName, s)
contentTypeHtml = ("Content-Type", "text/html")

todos :: Html
todos = H.docTypeHtml $ do
    H.head $ do
        H.title "Todos"
        htmxScriptTag
    H.body $ do
        h1 "todo list"
        ul $ forM_ (["1", "2", "3"]) (li . todoItem)

todoItem :: Text -> Html
todoItem n =
    let linkText = "todo item " <> toHtml n
        linkAddress = "/todos/" <> toValue n
     in H.div
            ( a linkText
                ! href linkAddress
                ! hxSwap "outerHTML"
                ! hxTarget "closest ul"
            )
            ! hxBoost

data TodoItem = TodoItem
    { identifier :: Text
    , value :: Text
    , completed :: Bool
    }

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application request respond =
    case (requestMethod request, pathInfo request) of
        ("GET", ["todos"]) ->
            respond $
                responseLBS status200 [contentTypeHtml] $
                    renderHtml todos
        ("GET", ["todos", str]) ->
            respond $
                responseLBS status200 [contentTypeHtml] $
                    renderHtml $
                        todoItem str
        _ ->
            respond $
                responseLBS status200 [contentTypeHtml] $
                    renderHtml hello
