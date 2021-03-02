{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Rating where

import Beckn.Types.App (RatingId)
import Beckn.Types.ID
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import Data.Swagger (ToSchema)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude

data RatingT f = Rating
  { _id :: B.C f RatingId,
    _productInstanceId :: B.C f (ID ProductInstance),
    _ratingValue :: B.C f Int,
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Rating = RatingT Identity

type RatingPrimaryId = B.PrimaryKey RatingT Identity

instance B.Table RatingT where
  data PrimaryKey RatingT f = RatingPrimaryKey (B.C f RatingId)
    deriving (Generic, B.Beamable)
  primaryKey = RatingPrimaryKey . _id

deriving instance Show Rating

deriving instance Eq Rating

instance ToJSON Rating where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Rating where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Rating

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RatingT)
fieldEMod =
  B.setEntityName "rating"
    <> B.modifyTableFields
      B.tableModification
        { _productInstanceId = "product_instance_id",
          _ratingValue = "rating_value",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
