{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImplicitParams           #-}
{-# LANGUAGE OverloadedStrings        #-}

module Lib
    ( listen
    ) where

import           Control.Exception
import qualified Data.Aeson                       as Aeson
import           Data.Foldable
import           Data.Function
import           Data.Int                         (Int64)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import qualified Data.Pool                        as P
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as DT
import           Data.Traversable
import           Data.UUID
import           Data.UUID.V4                     (nextRandom)
import qualified Database.PostgreSQL.Simple       as DB
import           Database.PostgreSQL.Simple.ToRow
import qualified Network.HTTP.Types               as HTTPTypes
import qualified Network.Wai                      as Wai
import qualified Network.Wai.Application.Static   as WaiStatic
import qualified Network.Wai.Handler.Warp         as Warp

import           Model.Group
import           Model.User


app :: (?pool :: (P.Pool DB.Connection)) => Wai.Application
app req res = withDb $ \conn -> do
  let ?conn = conn
  case Wai.pathInfo req of
        ["user", id, "groups"] -> routeWithId id fetchGroupsOfUser req res
        ["user", id]           -> routeWithId id fetchUser req res
        ["users"]              -> fetchAllUsers >>= res . jsonRoute
        ["groups"]             -> fetchGroupsById >>= res . jsonRoute
        ["health"]             -> res healthRoute
        _                      -> staticRoute req res


staticRoute :: Wai.Application
staticRoute = WaiStatic.staticApp $ WaiStatic.defaultFileServerSettings "./public"

routeWithId :: Aeson.ToJSON a => Text -> (UUID -> IO a) -> Wai.Application
routeWithId id fetch req res = case fromText id of
  Just userId -> fetch userId >>= res . jsonRoute
  Nothing     -> res $ jsonRoute ("Invalid uuid!" :: Text)


basicLogger :: Wai.Middleware
basicLogger app req res = do
  print $ Wai.requestMethod req
  app req res

detailedLogger :: Wai.Middleware
detailedLogger app req res = do
  print req
  app req res

middlewareChain :: [Wai.Middleware]
middlewareChain = [basicLogger, detailedLogger]

route = Wai.responseLBS
        HTTPTypes.status200
        [("Content-Type", "application/json")]

jsonRoute :: Aeson.ToJSON a => a -> Wai.Response
jsonRoute = route . Aeson.encode


anyRoute :: Wai.Response
anyRoute = jsonRoute ("Welcome to Secret Santa!" :: Text)

healthRoute = jsonRoute ("I'm fine" :: Text)

listen :: IO ()
listen = do
  connectionPool <- winterPool
  let ?pool = connectionPool
  res <- try (withinTransaction $ \conn -> do
    let ?conn = conn
    jannicId <- createUser "Jannic Beck" "jannicbeck@gmail.com"
    nicoId <- createUser "Nicolas Beck" "nico151089@gmail.com"
    createGroup "Festivus" "A festivus for the rest of us" nicoId $ Set.fromList [jannicId, nicoId]) :: IO (Either SomeException UUID)
  case res of
    Left e        -> putStrLn $ "Failed to create group \n" ++ show e
    Right groupId -> withDb $ \conn -> do
      let ?conn = conn
      justInsertedGroup <- fetchGroup groupId
      putStrLn $ "You just created: " ++ show justInsertedGroup
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

withDb :: (?pool :: (P.Pool DB.Connection)) => (DB.Connection -> IO c) -> IO c
withDb = P.withResource ?pool

withinTransaction :: (?pool :: (P.Pool DB.Connection)) => (DB.Connection -> IO c) -> IO c
withinTransaction lambda = withDb $ \conn -> DB.withTransaction conn $ lambda conn

createUser :: (?conn :: DB.Connection) => Text -> Text -> IO UUID
createUser name mail = do
  userId <- nextRandom
  let user = User userId name mail
  DB.execute ?conn "insert into winter.users (id, name, email) values (?, ?, ?)" user
  return userId

createGroup :: (?conn :: DB.Connection) => Text -> Text -> UUID -> Set UUID -> IO UUID
createGroup name description creatorId userIds = do
  groupId <- nextRandom
  DB.execute ?conn "insert into winter.groups (id, name, description, creator_id) values (?, ?, ?, ?)" (groupId, name, description, creatorId)
  --TODO replace with executeMany
  forM_ (Set.insert creatorId userIds) $ \userId -> do
    memberShipId <- nextRandom
    DB.execute ?conn "insert into winter.group_members (id, group_id, user_id) values (?, ?, ?)" (memberShipId, groupId, userId)
  return groupId

fetchUser :: (?conn :: DB.Connection) => UUID -> IO User
fetchUser id = head <$> fetchUsers [id]

fetchUsers :: (?conn :: DB.Connection) => [UUID] -> IO [User]
fetchUsers ids = DB.query ?conn "select id, name, email from winter.users u where u.id in ?" $ DB.Only $ DB.In ids

fetchAllUsers :: (?conn :: DB.Connection) => IO [User]
fetchAllUsers = DB.query_ ?conn "select * from winter.users" :: IO [User]

fetchGroupsById :: (?conn :: DB.Connection) => IO GroupsById
fetchGroupsById = do
  results <- DB.query_
    ?conn
    "select user_id, u.name as u_name, email, group_id, creator_id, g.name as g_name, description from users u join group_members gm on u.id = gm.user_id join groups g on g.id = gm.group_id"
    :: IO [(UUID, Text, Text, UUID, UUID, Text, Text)]
  return $ getGroupsById results

getGroupsById :: [(UUID, Text, Text, UUID, UUID, Text, Text)] -> GroupsById
getGroupsById = foldl (\result (uId, uName, uMail, gId, creatorId, gName, gDescr) ->
  if Map.notMember gId result then
    Map.insert
      gId
      (Group gId gName gDescr creatorId $ Set.fromList [User uId uName uMail])
      result
  else
    Map.adjust
      (\g -> g { members = Set.insert (User uId uName uMail) (members g) })
      gId
      result
  ) (Map.empty :: GroupsById)

fetchGroup :: (?conn :: DB.Connection) => UUID -> IO Group
fetchGroup groupId = do
  [(name, description, creatorId)] <- (DB.query ?conn "select name, description, creator_id from winter.groups g where g.id = ?" $ DB.Only groupId) :: IO [(Text, Text, UUID)]
  memberIds <- (DB.query ?conn "select user_id from winter.group_members m where m.group_id = ?" $ DB.Only groupId) :: IO [DB.Only UUID]
  users <- fetchUsers $ DB.fromOnly <$> memberIds
  return (Group groupId name description creatorId (Set.fromList users))

fetchGroupsOfUser :: (?conn :: DB.Connection) => UUID -> IO [Group]
fetchGroupsOfUser userId = do
  results <- DB.query
    ?conn
    "select group_id from group_members gm join users u on u.id = gm.user_id where user_id = ?" $ DB.Only userId
    :: IO [DB.Only UUID]
  forM results $ \(DB.Only gId) -> fetchGroup gId


winterPool :: IO (P.Pool DB.Connection)
winterPool = P.createPool
  connectDb
  DB.close
  1 -- stripes
  10 -- unused connections are kept open for 10 seconds
  10 -- max. 10 connections open per stripe
