{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( listen
    ) where

import           Data.Aeson
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.HTTP.Types         (status200)
import           Network.Wai                (Application, pathInfo, responseLBS)
import           Network.Wai.Handler.Warp   (run)

data User = User { name :: Text, email :: Text }
          deriving (Generic, Show)

instance ToJSON User where
  -- No need to provide a toJSON implementation.

  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON User
  -- No need to provide a parseJSON implementation.

user = User { name = "Jannic Beck", email = "jannicbeck@gmail.com" }

-- somehow putStrLn is executed twice when visiting a route
app :: Application
app req res = do
    putStrLn "I've done some IO here"
    res $
      case pathInfo req of
        ["api"]    -> secretSantaRoute
        ["health"] -> healthRoute
        _          -> anyRoute

route = responseLBS
        status200
        [("Content-Type", "application/json")]

anyRoute = route (encode ("Welcome to Secret Santa!" :: Text))
secretSantaRoute = route (encode user)
healthRoute = route (encode ("I'm fine" :: Text))

listen :: IO ()
listen = do
  port <- connectDb
  putStrLn ("Listening on port " ++ show port)
  run port app


connectDb :: IO Int
connectDb = do
  conn <- connectPostgreSQL "host=localhost port=5432 dbname=winter-db user=winter password=winter"
  [Only i] <- query_ conn "select 2000 + 1000"
  return i
