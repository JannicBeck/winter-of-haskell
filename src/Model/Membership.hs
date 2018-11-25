{-# LANGUAGE DeriveGeneric #-}

module Model.Membership
    ( Membership(..)
    ) where

import qualified Data.Aeson                         as Aeson
import           Data.UUID
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics

data Membership = Membership { _id :: UUID, groupId :: UUID, userId :: UUID }
           deriving (Generic, Show)

instance Aeson.ToJSON Membership where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Membership

instance FromRow Membership where
    fromRow = Membership <$> field <*> field <*> field

instance ToRow Membership where
    toRow g = [toField $ Model.Membership._id g, toField $ groupId g, toField $ userId g]
