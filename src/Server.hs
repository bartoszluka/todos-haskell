module Server (
    TodoItem (..),
    application,
    appWithState,
) where

import Control.Concurrent (modifyMVar, readMVar)
import Control.Concurrent.MVar (MVar)
import Data.ByteString qualified as B
import Data.Text (Text)
import Foundation
import Foundation.Collection (forM_)
import Network.HTTP.Types (HeaderName, status200)
import Network.Wai
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx
import Prelude (print)

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

todos :: [TodoItem] -> Html
todos todoList = H.docTypeHtml $ do
    H.head $ do
        H.title "Todos"
        -- styleSheet "style.css"
        htmxScriptTag
    H.body $ do
        h1 "todo list"
        ul $ forM_ todoList (li . todoItem)

data TodoItem = TodoItem
    { identifier :: Text
    , description :: Text
    , completed :: Bool
    }
    deriving (Show, Eq)

todoItem :: TodoItem -> Html
todoItem TodoItem{identifier, description, completed} =
    H.div $
        mconcat
            [ H.span (text description)
            , input
                ! type_ "checkbox"
                ! name "completed"
                ! hxPut ("/todos/" <> toValue identifier)
                ! value "true"
                ! if completed then checked "true" else mempty
            , button ! hxDelete ("/todos/" <> toValue identifier) $ text "delete"
            ]

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application request respond =
    case (requestMethod request, pathInfo request) of
        ("GET", ["todos"]) ->
            respond $
                responseLBS status200 [contentTypeHtml] $
                    renderHtml $ todos []
        ("GET", ["style.css"]) ->
            respond $
                responseFile status200 [contentTypeCss] "static/css/style.css" Nothing
        ("GET", ["htmx.min.js"]) ->
            respond $
                responseFile status200 [contentTypeJavaScript] "static/htmx-v1.9.10-min.js" Nothing
        ("GET", ["favicon.ico"]) ->
            respond $
                responseFile status200 [contentTypeJavaScript] "static/favicon.ico" Nothing
        ("PUT", ["todos", todoId]) -> do
            print todoId
            rqBody <- getRequestBodyChunk request
            let newTodo = case rqBody of
                    "" -> TodoItem{identifier = todoId, description = "dupa", completed = False}
                    str
                        | "completed=" `B.isPrefixOf` str ->
                            TodoItem{identifier = todoId, description = "dupa", completed = True}
                    _ -> error "dupa"

            print newTodo
            respond $
                responseLBS status200 [("Content-Type", "text/plain")] ""
        ("DELETE", ["todos", todoId]) -> do
            Prelude.print todoId
            rqBody <- getRequestBodyChunk request
            let newTodo = case rqBody of
                    "" -> TodoItem{identifier = todoId, description = "dupa", completed = False}
                    str
                        | "completed=" `B.isPrefixOf` str ->
                            TodoItem{identifier = todoId, description = "dupa", completed = True}
                    _ -> error "dupa"

            Prelude.print newTodo
            respond $
                responseLBS status200 [("Content-Type", "text/plain")] ""
        other -> do
            Prelude.print other
            rqBody <- getRequestBodyChunk request
            Prelude.print rqBody
            respond $
                responseLBS status200 [contentTypeHtml] $
                    renderHtml hello

appWithState :: MVar [TodoItem] -> Request -> (Response -> IO b) -> IO b
appWithState todoList request respond = do
    case (requestMethod request, pathInfo request) of
        ("GET", ["todos"]) -> do
            todoList' <- readMVar todoList
            respond $
                responseLBS status200 [contentTypeHtml] $
                    renderHtml $
                        todos todoList'
        ("GET", ["style.css"]) ->
            respond $
                responseFile status200 [contentTypeCss] "static/css/style.css" Nothing
        ("GET", ["htmx.min.js"]) ->
            respond $
                responseFile status200 [contentTypeJavaScript] "static/htmx-v1.9.10-min.js" Nothing
        ("GET", ["favicon.ico"]) ->
            respond $
                responseFile status200 [contentTypeJavaScript] "static/favicon.ico" Nothing
        ("PUT", ["todos", todoId]) -> do
            rqBody <- getRequestBodyChunk request
            let isCompleted = case rqBody of
                    "" -> False
                    str | "completed=" `B.isPrefixOf` str -> True
                    _ -> error "dupa"

            modifyMVar todoList $ \todoList' -> do
                let changeCompleted item' =
                        if identifier item' == todoId
                            then item'{completed = isCompleted}
                            else item'
                    new = changeCompleted <$> todoList'

                responseReceived <- respond $ responseLBS status200 [("Content-Type", "text/plain")] ""
                return (new, responseReceived)
        -- print todoList
        _ -> undefined
