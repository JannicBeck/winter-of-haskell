{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( listen
    ) where

import           Control.Exception
import qualified Data.Aeson                     as Aeson
import qualified Data.Set                       as Set
import           Data.Text
import qualified Data.Time                      as Time
import qualified Database.PostgreSQL.Simple     as DB
import           GHC.Generics
import qualified Network.HTTP.Types             as HTTPTypes
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp       as Warp

data User = User { userName :: Text, userEmail :: Text }
          deriving (Generic, Show, Eq, Ord)


instance Aeson.ToJSON User where
  -- No need to provide a toJSON implementation.

  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON User
  -- No need to provide a parseJSON implementation.

data Group = Group { groupName :: Text, groupMembers :: Set.Set User }
           deriving (Generic, Show)
data GroupOptions = GroupOptions { giftCostLimit :: Maybe Text, dateOfDrawing :: Maybe Time.ZonedTime }
                  deriving (Generic, Show)

instance Aeson.ToJSON Group where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Group

jannic = User { userName = "Jannic Beck", userEmail = "jannicbeck@gmail.com" }
jj = User { userName = "Jannic Beck", userEmail = "jannicbeck@googlemail.com" }
jb = User { userName = "Jannic Back", userEmail = "jannicbeck@gmail.com" }
nico = User { userName = "Nicolas Beck", userEmail = "nico1510@gmail.com" }

g = Group { groupName = "Christmas", groupMembers = Set.fromList[jannic, nico, jb, jj] }

app :: Wai.Application
app req res = case Wai.pathInfo req of
        ["users"]  -> res usersRoute
        ["groups"] -> res groupsRoute
        ["health"] -> res healthRoute
        _          -> staticApp req res

staticApp :: Wai.Application
staticApp = WaiStatic.staticApp $ WaiStatic.defaultFileServerSettings "./public"


route = Wai.responseLBS
        HTTPTypes.status200
        [("Content-Type", "application/json")]

anyRoute = route (Aeson.encode ("Welcome to Secret Santa!" :: Text))
usersRoute = route (Aeson.encode jannic)
groupsRoute = route (Aeson.encode g)
healthRoute = route (Aeson.encode ("I'm fine" :: Text))

listen :: IO ()
listen = do
  port <- connectDb
  putStrLn $ "Listening on port " ++ show port
  Warp.run port app

connectDb :: IO Int
connectDb = try (DB.connectPostgreSQL "host=localhost port=5432 dbname=winter-db user=winter password=winter")
   >>= \maybeConn ->
          case (maybeConn :: Either SomeException DB.Connection) of
              Left e     -> useDefaultPort e
              Right conn -> queryPort conn

useDefaultPort :: SomeException -> IO Int
useDefaultPort e = putStrLn (show e)
                  >> putStrLn ("Connection to db failed. Falling back to default port " ++ show defaultPort)
                  >> return defaultPort
                  where defaultPort = 3002

queryPort :: DB.Connection -> IO Int
queryPort conn = DB.query_ conn "select 2000 + 1000"
                 >>= \([DB.Only port]) -> return port
