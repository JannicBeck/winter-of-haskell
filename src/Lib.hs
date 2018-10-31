{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( listen
    ) where

import           Data.Aeson
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import           Data.Time                (ZonedTime)
import           GHC.Generics
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, pathInfo, responseLBS)
import           Network.Wai.Handler.Warp (run)


data User = User { userName :: Text, userEmail :: Text }
          deriving (Generic, Show, Eq, Ord)

instance ToJSON User where
  -- No need to provide a toJSON implementation.

  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON User
  -- No need to provide a parseJSON implementation.

data Group = Group { groupName :: Text, groupMembers :: Set.Set User }
           deriving (Generic, Show)
data GroupOptions = GroupOptions { giftCostLimit :: Maybe Text, dateOfDrawing :: Maybe ZonedTime }
                  deriving (Generic, Show)

instance ToJSON Group where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Group

jannic = User { userName = "Jannic Beck", userEmail = "jannicbeck@gmail.com" }
nico = User { userName = "Nicolas Beck", userEmail = "nico1510@gmail.com" }

g = Group { groupName = "Christmas", groupMembers = Set.fromList([jannic, nico]) }

-- somehow putStrLn is executed twice when visiting a route
app :: Application
app req res = do
    putStrLn "I've done some IO here"
    res $
      case pathInfo req of
        ["users"]  -> usersRoute
        ["groups"] -> groupsRoute
        ["health"] -> healthRoute
        _          -> anyRoute

route = responseLBS
        status200
        [("Content-Type", "application/json")]

anyRoute = route (encode ("Welcome to Secret Santa!" :: Text))
usersRoute = route (encode jannic)
groupsRoute = route (encode g)
healthRoute = route (encode ("I'm fine" :: Text))

listen :: IO ()
listen = do
  let port = 3000
  putStrLn ("Listening on port " ++ show port)
  run port app
