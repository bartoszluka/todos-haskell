module Server (
    TodoItem (..),
    appWithState,
) where

import Control.Concurrent (modifyMVar, readMVar)
import Control.Concurrent.MVar (MVar)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding qualified as TSE
import Foundation
import Foundation.Collection (forM_)
import Network.HTTP.Types (HeaderName, status200, status404, urlDecode)
import Network.Wai
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx
import Prelude (print)
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

todos :: [TodoItem] -> Html
todos todoList = H.docTypeHtml $ do
    H.head $ do
        H.title "Todos"
        styleSheet "style.css"
        htmxScriptTag
    H.body $ do
        h1 "todo list"
        ul ! A.id "todo-list" $ forM_ todoList (li . todoItem)

        input
            ! type_ "text"
            ! name "task"
            ! placeholder "very important task"
            ! hxPost "/todos"
            ! hxTarget "#todo-list"
            ! hxSwap "beforeend"

data TodoItem = TodoItem
    { identifier :: Text
    , description :: Text
    , completed :: Bool
    }
    deriving (Show, Eq)

hxSwapOuter :: Attribute
hxSwapOuter = hxSwap "outerHTML"

todoItem :: TodoItem -> Html
todoItem TodoItem{identifier, description, completed} =
    H.div ! class_ "todo-item" $
        mconcat
            [ H.span (text description)
                ! if completed then class_ "completed" else mempty
            , input
                ! type_ "checkbox"
                ! name "completed"
                ! hxPut ("/todos/" <> toValue identifier)
                ! hxSwapOuter
                ! hxTarget "closest .todo-item"
                ! if completed then checked "true" else mempty
            , button
                ! hxDelete ("/todos/" <> toValue identifier)
                ! hxTarget "closest li"
                ! hxSwapOuter
                $ text "delete"
            ]

changeCompletion :: MVar [TodoItem] -> Text -> Bool -> IO (Maybe TodoItem)
changeCompletion todoList todoId isCompleted = modifyMVar todoList $ \todoList' -> do
    let changeCompleted item' =
            if identifier item' == todoId
                then item'{completed = isCompleted}
                else item'
        newState = changeCompleted <$> todoList'
        newTodo = find ((== todoId) . identifier) newState

    return (newState, newTodo)

appendItem :: MVar [a] -> a -> IO ()
appendItem mvar todo = modifyMVar mvar $
    \todoList -> return (todoList Prelude.++ [todo], ())

emptyOk :: Response
emptyOk = responseLBS status200 [("Content-Type", "text/plain")] ""

textToByteString :: Text -> ByteString
textToByteString = TSE.encodeUtf8

byteStringToText :: ByteString -> Text
byteStringToText = TSE.decodeUtf8

appWithState :: MVar [TodoItem] -> Request -> (Response -> IO b) -> IO b
appWithState todoList request respond = do
    let respondHtml = respond . responseLBS status200 [contentTypeHtml] . renderHtml
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
                    _ -> True

            mbyTodo <- changeCompletion todoList todoId isCompleted
            print mbyTodo
            respond $ case mbyTodo of
                Just newTodo -> responseLBS status200 [contentTypeHtml] $ renderHtml $ todoItem newTodo
                Nothing -> responseLBS status404 [("Content-Type", "text/plain")] $ "the todo item with ID " <> (B.fromStrict . textToByteString $ todoId) <> " not found"
        ("POST", ["todos"]) -> do
            rqBody <- urlDecode False <$> getRequestBodyChunk request
            let newTask = case B.stripPrefix "task=" rqBody of
                    Nothing -> error $ show rqBody
                    Just "" -> undefined
                    Just bstr ->
                        let str = byteStringToText bstr
                         in TodoItem{identifier = str, description = str, completed = False}

            print newTask
            appendItem todoList newTask
            respondHtml $ li . todoItem $ newTask
        ("DELETE", ["todos", todoId]) -> do
            modifyMVar todoList $ \todoList' -> do
                let newState = filter (\item' -> identifier item' /= todoId) todoList'

                responseReceived <- respond $ responseLBS status200 [("Content-Type", "text/plain")] ""
                return (newState, responseReceived)
        other -> do
            Prelude.print other
            rqBody <- getRequestBodyChunk request
            Prelude.print rqBody
            respond $
                responseLBS status200 [contentTypeHtml] $
                    renderHtml hello
