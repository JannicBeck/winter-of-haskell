{-# LANGUAGE DeriveGeneric #-}

module Model.Group
    ( Group(..)
    ) where

import qualified Data.Aeson   as Aeson
import           Data.Set     (Set)
import           Data.Text    (Text)
import qualified Data.Text    as DT
import           Data.Time    (ZonedTime)
import           GHC.Generics
import           Model.User


data Group = Group { _id :: Text, groupName :: Text, groupMembers :: Set User }
           deriving (Generic, Show)
data GroupOptions = GroupOptions { giftCostLimit :: Maybe Text, dateOfDrawing :: Maybe ZonedTime }
                  deriving (Generic, Show)

instance Aeson.ToJSON Group where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Group
