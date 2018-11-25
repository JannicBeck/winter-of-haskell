{-# LANGUAGE DeriveGeneric #-}

module Model.Group
    ( Group(..)
    ) where

import qualified Data.Aeson                         as Aeson
import           Data.Set
import           Data.Text                          (Text)
import qualified Data.Text                          as DT
import           Data.Time                          (ZonedTime)
import           Data.UUID
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Model.User

data Group = Group { _id :: UUID, name :: Text, description :: Text, creatorId :: UUID, members :: Set User }
           deriving (Generic, Show)
data GroupOptions = GroupOptions { giftCostLimit :: Maybe Text, dateOfDrawing :: Maybe ZonedTime }
                  deriving (Generic, Show)

instance Aeson.ToJSON Group where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Group

instance FromRow Group where
    fromRow = Group <$> field <*> field <*> field <*> field <*> pure (fromList [])

instance ToRow Group where
    toRow g = [toField $ Model.Group._id g, toField $ name g, toField $ description g, toField $ creatorId g]
