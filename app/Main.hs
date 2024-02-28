import Foundation (IO)
import Lib (application)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 application
