{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( listen
    ) where

import           Control.Exception
import qualified Data.Aeson                     as Aeson
import           Data.Function
import qualified Data.Set                       as Set
import qualified Data.Text                      as DT
import qualified Data.Time                      as Time
import qualified Database.PostgreSQL.Simple     as DB
import           GHC.Generics
import qualified Network.HTTP.Types             as HTTPTypes
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp       as Warp

data User = User { userName :: DT.Text, userEmail :: DT.Text }
          deriving (Generic, Show, Eq, Ord)


instance Aeson.ToJSON User where
  -- No need to provide a toJSON implementation.

  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON User
  -- No need to provide a parseJSON implementation.

data Group = Group { groupName :: DT.Text, groupMembers :: Set.Set User }
           deriving (Generic, Show)
data GroupOptions = GroupOptions { giftCostLimit :: Maybe DT.Text, dateOfDrawing :: Maybe Time.ZonedTime }
                  deriving (Generic, Show)

instance Aeson.ToJSON Group where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Group

jannic = User { userName = "Jannic Beck", userEmail = "jannicbeck@gmail.com" }
jj = User { userName = "Jannic Beck", userEmail = "jannicbeck@googlemail.com" }
jb = User { userName = "Jannic Back", userEmail = "jannicbeck@gmail.com" }
nico = User { userName = "Nicolas Beck", userEmail = "nico1510@gmail.com" }

g = Group { groupName = "Christmas", groupMembers = Set.fromList [jannic, nico, jb, jj] }

app :: Wai.Application
app req res = case Wai.pathInfo req of
        ["users"]  -> res usersRoute
        ["groups"] -> res groupsRoute
        ["health"] -> res healthRoute
        _          -> staticApp req res

staticApp :: Wai.Application
staticApp = WaiStatic.staticApp $ WaiStatic.defaultFileServerSettings "./public"


basicLogger :: Wai.Middleware
basicLogger app req res = do
  print $ Wai.requestMethod req
  app req res

detailedLogger :: Wai.Middleware
detailedLogger app req res = do
  print req
  app req res

middlewareChain = [basicLogger, detailedLogger]

route = Wai.responseLBS
        HTTPTypes.status200
        [("Content-Type", "application/json")]

jsonRoute :: Aeson.ToJSON a => a -> Wai.Response
jsonRoute = route . Aeson.encode

anyRoute = jsonRoute ("Welcome to Secret Santa!" :: DT.Text)
usersRoute = jsonRoute jannic
groupsRoute = jsonRoute g
healthRoute = jsonRoute ("I'm fine" :: DT.Text)

listen :: IO ()
listen = do
  port <- queryPort
  putStrLn $ "Listening on port " ++ show port
  Warp.run port $ foldr ($) app middlewareChain

connectInfo :: DB.ConnectInfo
connectInfo = DB.ConnectInfo {
      DB.connectHost = "localhost"
    , DB.connectPort = 5432
    , DB.connectUser = "winter"
    , DB.connectPassword = "winter"
    , DB.connectDatabase = "winter-db"
    }

connectDb :: IO DB.Connection
connectDb = DB.connectPostgreSQL $ DB.postgreSQLConnectionString connectInfo

queryPort :: IO Int
queryPort = do
  res <- try $ connectDb
        >>= (\conn -> DB.query_ conn "select 2000 + 1002") :: IO (Either SomeException [DB.Only Int])
  case res of
      Left e -> putStrLn (show e ++ "\nConnection to db failed. Falling back to default port " ++ show defaultPort)
                >> return defaultPort where defaultPort = 3002
      Right [DB.Only port] -> return port
