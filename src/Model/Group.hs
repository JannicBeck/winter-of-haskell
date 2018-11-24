{-# LANGUAGE DeriveGeneric #-}

module Model.Group
    ( Group(..),
      GroupsById
    ) where

import qualified Data.Aeson   as Aeson
import           Data.Map     (Map)
import           Data.Set     (Set)
import           Data.Text    (Text)
import qualified Data.Text    as DT
import           Data.Time    (ZonedTime)
import           Data.UUID
import           GHC.Generics
import           Model.User

type GroupsById = Map UUID Group

data Group = Group { _id :: UUID, name :: Text, description :: Text, creatorId :: UUID, members :: Set User }
           deriving (Generic, Show)
data GroupOptions = GroupOptions { giftCostLimit :: Maybe Text, dateOfDrawing :: Maybe ZonedTime }
                  deriving (Generic, Show)

instance Aeson.ToJSON Group where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Group

