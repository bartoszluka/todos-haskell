import Foundation
import Lib (application)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    putStrLn $ "listening on port: " <> show port
    run port application
