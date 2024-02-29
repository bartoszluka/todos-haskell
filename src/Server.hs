module Server (
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
import Prelude qualified

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
        ! src "htmx.min.js"

styleSheet :: Text -> Html
styleSheet css = link ! rel "stylesheet" ! href (toValue css)

contentTypeHtml :: (IsString s) => (HeaderName, s)
contentTypeHtml = ("Content-Type", "text/html")

contentTypeJavaScript :: (IsString s) => (HeaderName, s)
contentTypeJavaScript = ("Content-Type", "application/javascript")

contentTypeCss :: (IsString s) => (HeaderName, s)
contentTypeCss = ("Content-Type", "text/css")

todos :: Html
todos = H.docTypeHtml $ do
    H.head $ do
        H.title "Todos"
        -- styleSheet "style.css"
        htmxScriptTag
    H.body $ do
        h1 "todo list"
        ul $ forM_ examples (li . todoItem)
  where
    examples = [TodoItem "1" "todo1" True, TodoItem "2" "todo2" False]

toText :: Int -> Text
toText = T.pack . Prelude.show

data TodoItem = TodoItem
    { identifier :: Text
    , description :: Text
    , completed :: Bool
    }

todoItem :: TodoItem -> Html
todoItem TodoItem{identifier, description, completed} =
    H.div $
        mconcat
            [ H.span (text description)
            , input ! type_ "checkbox" ! hxPut ("/todos/" <> toValue identifier) ! value "dupa"
            , button ! hxDelete ("/todos/" <> toValue identifier) $ text "delete"
            ]

-- <button hx-post="/clicked"
--     hx-trigger="click"
--     hx-target="#parent-div"
--     hx-swap="outerHTML"
-- >
--     Click Me!
-- </button>

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
        ("GET", ["style.css"]) ->
            respond $
                responseFile status200 [contentTypeCss] "static/css/style.css" Nothing
        ("GET", ["htmx.min.js"]) ->
            respond $
                responseFile status200 [contentTypeJavaScript] "static/htmx-v1.9.10-min.js" Nothing
        ("GET", ["favicon.ico"]) ->
            respond $
                responseFile status200 [contentTypeJavaScript] "static/favicon.ico" Nothing
        other -> do
            Prelude.print other
            respond $
                responseLBS status200 [contentTypeHtml] $
                    renderHtml hello
