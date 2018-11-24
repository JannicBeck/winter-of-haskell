{-# LANGUAGE DisambiguateRecordFields #-}
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


app :: Wai.Application
app req res = case Wai.pathInfo req of
        ["user", id, "groups"] -> routeWithId id getGroupsOfUser req res
        ["user", id]           -> routeWithId id getUser req res
        ["users"]              -> getAllUsers >>= res . jsonRoute
        ["groups"]             -> getAllGroups >>= res . jsonRoute
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

middlewareChain = [basicLogger, detailedLogger]

route = Wai.responseLBS
        HTTPTypes.status200
        [("Content-Type", "application/json")]

jsonRoute :: Aeson.ToJSON a => a -> Wai.Response
jsonRoute = route . Aeson.encode


anyRoute = jsonRoute ("Welcome to Secret Santa!" :: Text)

getById :: Aeson.ToJSON a => (DB.Connection -> UUID -> IO a) -> UUID -> IO a
getById fetch id = withDb $ \conn -> fetch conn id

getGroupsOfUser = getById fetchGroupsOfUser
getUser = getById fetchUser

getAllUsers = withDb $ \conn -> fetchAllUsers conn
getAllGroups = withDb $ \conn -> fetchGroupsById conn

healthRoute = jsonRoute ("I'm fine" :: Text)

listen :: IO ()
listen = do
  res <- try (withinTransaction $ \conn -> do
    jannicId <- createUser conn "Jannic Beck" "jannicbeck@gmail.com"
    nicoId <- createUser conn "Nicolas Beck" "nico151089@gmail.com"
    createGroup conn "Festivus" "A festivus for the rest of us" nicoId $ Set.fromList [jannicId, nicoId]) :: IO (Either SomeException UUID)
  case res of
    Left e        -> putStrLn $ "Failed to create group \n" ++ show e
    Right groupId -> withDb $ \conn -> do
      justInsertedGroup <- fetchGroup conn groupId
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

withDb :: (DB.Connection -> IO c) -> IO c
withDb = bracket connectDb DB.close

withinTransaction :: (DB.Connection -> IO c) -> IO c
withinTransaction lambda = withDb $ \conn -> DB.withTransaction conn $ lambda conn

createUser :: DB.Connection -> Text -> Text -> IO UUID
createUser conn name mail = do
  userId <- nextRandom
  let user = User userId name mail
  DB.execute conn "insert into winter.users (id, name, email) values (?, ?, ?)" user
  return userId

createGroup :: DB.Connection -> Text -> Text -> UUID -> Set UUID -> IO UUID
createGroup conn name description creatorId userIds = do
  groupId <- nextRandom
  DB.execute conn "insert into winter.groups (id, name, description, creator_id) values (?, ?, ?, ?)" (groupId, name, description, creatorId)
  --TODO replace with executeMany
  forM_ (Set.insert creatorId userIds) $ \userId -> do
    memberShipId <- nextRandom
    DB.execute conn "insert into winter.group_members (id, group_id, user_id) values (?, ?, ?)" (memberShipId, groupId, userId)
  return groupId

fetchUser :: DB.Connection -> UUID -> IO User
fetchUser conn id = head <$> fetchUsers conn [id]

fetchUsers :: DB.Connection -> [UUID] -> IO [User]
fetchUsers conn ids = DB.query conn "select id, name, email from winter.users u where u.id in ?" $ DB.Only $ DB.In ids

fetchAllUsers :: DB.Connection -> IO [User]
fetchAllUsers conn = DB.query_ conn "select * from winter.users" :: IO [User]

fetchGroupsById :: DB.Connection -> IO GroupsById
fetchGroupsById conn = do
  results <- DB.query_
    conn
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

fetchGroup :: DB.Connection -> UUID -> IO Group
fetchGroup conn groupId = do
  [(name, description, creatorId)] <- (DB.query conn "select name, description, creator_id from winter.groups g where g.id = ?" $ DB.Only groupId) :: IO [(Text, Text, UUID)]
  memberIds <- (DB.query conn "select user_id from winter.group_members m where m.group_id = ?" $ DB.Only groupId) :: IO [DB.Only UUID]
  users <- fetchUsers conn $ DB.fromOnly <$> memberIds
  return (Group groupId name description creatorId (Set.fromList users))

fetchGroupsOfUser :: DB.Connection -> UUID -> IO [Group]
fetchGroupsOfUser conn userId = do
  results <- DB.query
    conn
    "select group_id from group_members gm join users u on u.id = gm.user_id where user_id = ?" $ DB.Only userId
    :: IO [DB.Only UUID]
  forM results $ \(DB.Only gId) -> fetchGroup conn gId
