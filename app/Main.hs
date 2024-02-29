import Control.Concurrent (newMVar)
import Foundation
import Network.Wai.Handler.Warp (run)
import Server (TodoItem (TodoItem), appWithState)

-- main :: IO ()
-- main = do
--     let port = 3000
--     putStrLn $ "listening on port: " <> show port
--     run port application
--
main :: IO ()
main = do
    let port = 3000
    putStrLn $ "listening on port: " <> show port
    todos <- newMVar [TodoItem "learn-haskell" "Learn Haskell" False, TodoItem "learn-server" "Learn Server" True]
    run port $ appWithState todos
