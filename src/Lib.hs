{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Lib
    ( listen
    ) where

import           Control.Exception
import qualified Data.Aeson                     as Aeson
import           Data.Function
import           Data.Int                       (Int64)
import qualified Data.Set                       as Set
import qualified Data.Text                      as DT
import           Data.UUID.V4                   as ID
import qualified Database.PostgreSQL.Simple     as DB
import qualified Network.HTTP.Types             as HTTPTypes
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp       as Warp

import           Model.Group
import           Model.User

jannic = User { _id = "mock-id1", userName = "Jannic Beck", userEmail = "jannicbeck@gmail.com" }
jj = User { _id = "mock-id2", userName = "Jannic Beck", userEmail = "jannicbeck@googlemail.com" }
jb = User { _id = "mock-id3", userName = "Jannic Back", userEmail = "jannicbeck@gmail.com" }
nico = User { _id = "mock-id4", userName = "Nicolas Beck", userEmail = "nico1510@gmail.com" }

g = Group { _id = "mock-group-id2", groupName = "Christmas", groupMembers = Set.fromList [jannic, nico, jb, jj] }

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
  createUser "Jannic Beck" "jannicbeck@gmail.com"
  putStrLn $ "Listening on port " ++ show port
  Warp.run port $ foldr ($) app middlewareChain
  where port = 3002

connectDb :: IO DB.Connection
connectDb = DB.connect DB.defaultConnectInfo {
      DB.connectHost = "localhost"
    , DB.connectPort = 5432
    , DB.connectUser = "winter"
    , DB.connectPassword = "winter"
    , DB.connectDatabase = "winter-db"
    }

withDb :: (DB.Connection -> IO c) -> IO c
withDb = bracket connectDb DB.close

withinTransaction :: (DB.Connection -> IO c) -> IO c
withinTransaction lambda = withDb $ \conn -> DB.withTransaction conn $ lambda conn

createUser :: DT.Text -> DT.Text -> IO ()
createUser name mail = do
  userId <- ID.nextRandom
  let randomUser = User { _id = show userId, userName = name, userEmail =  mail }
  res <- try $ withinTransaction $ \conn -> DB.execute conn "insert into winter.users (id, name, email) values (?, ?, ?)" randomUser
  case (res :: Either SomeException Int64) of
      Left e -> putStrLn ("Insert failed \n" ++ show e)
      Right affectedRows -> putStrLn $ "Insert successful, affected rows: " ++ show affectedRows
