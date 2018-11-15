{-# LANGUAGE DeriveGeneric #-}

module Model.User
    (
    User(..)
    ) where

import qualified Data.Aeson                           as Aeson
import           Data.Text                            (Text)
import qualified Data.Text                            as DT
import           Data.UUID
import qualified Database.PostgreSQL.Simple           as DB
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics

data User = User { _id :: String, userName :: Text, userEmail :: Text }
          deriving (Generic, Show, Eq, Ord)


instance Aeson.ToJSON User where
  -- No need to provide a toJSON implementation.

  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON User
  -- No need to provide a parseJSON implementation.

-- TODO:NBE this is a terrible hack because you don't  understand this fucking RowParser AT ALL!!!!!!
instance DB.FromRow User where
  fromRow = ((\a b c -> User (show a) b c) :: UUID -> Text -> Text -> User)<$> field <*> field <*> field

instance ToRow User where
    toRow u = [toField (_id u),toField (userName u), toField (userEmail u)]
