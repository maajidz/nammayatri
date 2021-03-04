{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Types.Storage.Quotation where

import Beckn.Types.ID
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude
import Servant.API

data Status = NEW | PENDING | EXPIRED | CONFIRMED | SYSTEM_CANCELLED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Status

instance FromHttpApiData Status where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data QuotationT f = Quotation
  { _id :: B.C f (ID Quotation),
    _leadsId :: B.C f Text,
    _amount :: B.C f Text,
    _organizationId :: B.C f Text,
    _status :: B.C f Status,
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Quotation = QuotationT Identity

type QuotationPrimaryKey = B.PrimaryKey QuotationT Identity

instance B.Table QuotationT where
  data PrimaryKey QuotationT f = QuotationPrimaryKey (B.C f (ID Quotation))
    deriving (Generic, B.Beamable)
  primaryKey = QuotationPrimaryKey . _id

deriving instance Show Quotation

deriving instance Eq Quotation

instance ToJSON Quotation where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Quotation where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Quotation

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity QuotationT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at",
        _updatedAt = "updated_at",
        _leadsId = "booking_reference_id",
        _organizationId = "organization_id"
      }
