{-# LANGUAGE DeriveGeneric #-}

module Model.Group
    ( Group(..)
    ) where

import qualified Data.Aeson                         as Aeson
import qualified Data.Set                           as Set
import qualified Data.Text                          as DT
import qualified Data.Time                          as Time
import qualified Database.PostgreSQL.Simple         as DB
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Model.User


data Group = Group { _id :: DT.Text, groupName :: DT.Text, groupMembers :: Set.Set User }
           deriving (Generic, Show)
data GroupOptions = GroupOptions { giftCostLimit :: Maybe DT.Text, dateOfDrawing :: Maybe Time.ZonedTime }
                  deriving (Generic, Show)

instance Aeson.ToJSON Group where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Group
