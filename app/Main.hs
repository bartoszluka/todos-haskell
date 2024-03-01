import Control.Concurrent (newMVar)
import Foundation
import Network.Wai.Handler.Warp (run)
import Server (TodoItem (TodoItem), appWithState)

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "listening on port: " <> show port
    todos <-
        newMVar
            [ TodoItem "learn-haskell" "Learn Haskell" True
            , TodoItem "learn-server" "Learn Wai/Warp" False
            , TodoItem "learn-htmx" "Learn HTMX" False
            , TodoItem "learn-css" "Learn CSS" False
            , TodoItem "learn-html" "Learn HTML" False
            , TodoItem "publish" "Publish the project" False
            ]
    run port $ appWithState todos
