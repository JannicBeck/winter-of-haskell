{-# LANGUAGE OverloadedStrings #-}
import Network.Wai (responseLBS, Application, pathInfo)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Data.Aeson (encode)
import Data.Text (Text)


-- somehow putStrLn is executed twice when visiting a route
app :: Application
app req res = do
    putStrLn "I've done some IO here"
    res $
      case pathInfo req of
        ["hello"] -> helloRoute
        _ -> anyRoute

route = responseLBS
        status200
        [("Content-Type", "application/json")]

anyRoute = route (encode ("Hello there" :: Text))
helloRoute = route (encode ("Hello World" :: Text))

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app
