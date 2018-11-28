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
import           Model.Membership
import           Model.User


app :: Wai.Application
app req res = case Wai.pathInfo req of
        ["user", id, "groups"] -> routeWithId id getGroupsOfUser req res
        ["user", id]           -> routeWithId id getUser req res
        ["users"]              -> getAllUsers >>= res . jsonRoute
        ["groups"]             -> getGroups >>= res . jsonRoute
        ["groups", id]         -> routeWithId id getGroup req res
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

getById :: Aeson.ToJSON a => (DB.Connection -> UUID -> IO a) -> UUID -> IO a
getById fetch id = withDb $ \conn -> fetch conn id

getGroupsOfUser :: UUID -> IO [Group]
getGroupsOfUser = getById fetchGroupsOfUser

getUser :: UUID -> IO User
getUser = getById fetchUser

getGroup :: UUID -> IO Group
getGroup = getById fetchGroup

getGroups :: IO [Group]
getGroups = withDb fetchGroups

getAllUsers :: IO [User]
getAllUsers = withDb fetchAllUsers

healthRoute = jsonRoute ("I'm fine" :: Text)

listen :: IO ()
listen = do
  res <- try (withinTransaction $ \conn -> do
    jannic <- createUser conn "Jannic Beck" "jannicbeck@gmail.com"
    nico <- createUser conn "Nicolas Beck" "nico151089@gmail.com"
    groupId <- nextRandom
    let g = Group groupId "Festivus" "A festivus for the rest of us" (Model.User._id nico) (Set.fromList [jannic, nico])
    createGroup conn g) :: IO (Either SomeException Group)
  case res of
    Left e      -> putStrLn $ "Failed to create group \n" ++ show e
    Right group -> withDb $ \conn -> putStrLn $ "You just created: " ++ show group
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

createUser :: DB.Connection -> Text -> Text -> IO User
createUser conn name mail = do
  userId <- nextRandom
  let user = User userId name mail
  DB.execute conn "insert into winter.users (id, name, email) values (?, ?, ?)" user
  return user

createGroup :: DB.Connection -> Group -> IO Group
createGroup conn group = do
  DB.execute conn "insert into winter.groups (id, name, description, creator_id) values (?, ?, ?, ?)" group
  createMemberships conn group
  return group

createMemberships :: DB.Connection -> Group -> IO [Membership]
createMemberships conn group = do
  memberships <- mapM (createMembership group) $ toList $ members group
  DB.executeMany conn "insert into winter.group_members (id, group_id, user_id) values (?, ?, ?)" memberships
  return memberships

createMembership :: Group -> User -> IO Membership
createMembership g u = flip Membership (Model.Group._id g) (Model.User._id u) <$> nextRandom

fetchGroup :: DB.Connection -> UUID -> IO Group
fetchGroup conn groupId = do
  [group] <- DB.query conn "select id, name, description, creator_id from winter.groups g where g.id = ?" $ DB.Only groupId :: IO [Group]
  setGroupMembers conn group

fetchGroups :: DB.Connection -> IO [Group]
fetchGroups conn = do
  groupSummaries <- DB.query_ conn "select id, name, description, creator_id from winter.groups" :: IO [Group]
  mapM (setGroupMembers conn) groupSummaries

setGroupMembers :: DB.Connection -> Group -> IO Group
setGroupMembers conn g = do
  let groupId = Model.Group._id g -- look how stupid this is OLO
  memberIds <- (DB.query conn "select user_id from winter.group_members m where m.group_id = ?" $ DB.Only groupId) :: IO [DB.Only UUID]
  members <- fetchUsers conn $ DB.fromOnly <$> memberIds
  return g { members = Set.fromList members }

fetchUser :: DB.Connection -> UUID -> IO User
fetchUser conn id = head <$> fetchUsers conn [id]

fetchUsers :: DB.Connection -> [UUID] -> IO [User]
fetchUsers conn ids = DB.query conn "select id, name, email from winter.users u where u.id in ?" $ DB.Only $ DB.In ids

fetchAllUsers :: DB.Connection -> IO [User]
fetchAllUsers conn = DB.query_ conn "select * from winter.users" :: IO [User]

fetchGroupsOfUser :: DB.Connection -> UUID -> IO [Group]
fetchGroupsOfUser conn userId = do
  results <- DB.query
    conn
    "select group_id from group_members gm join users u on u.id = gm.user_id where user_id = ?" $ DB.Only userId
    :: IO [DB.Only UUID]
  forM (DB.fromOnly <$> results) $ fetchGroup conn

