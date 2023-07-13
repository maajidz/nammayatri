{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Utils where

import Data.ByteString.Internal (ByteString)
import Data.Time
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.DriverOffer as DomainDO
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.KVConnector.Types (MeshConfig (..))
import qualified EulerHS.Language as L
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Lib.Mesh as Mesh

defaultDate :: LocalTime
defaultDate =
  LocalTime
    { localDay = toEnum 1, --   :: Day,
      localTimeOfDay = defaultTimeOfDay --  :: TimeOfDay
    }

defaultTimeOfDay :: TimeOfDay
defaultTimeOfDay =
  TimeOfDay
    { todHour = 1, -- :: Int,-  range 0 - 23
      todMin = 1, -- :: Int, --  range 0 - 59
      -- Note that 0 <= 'todSec' < 61, accomodating leap seconds.
      -- Any local minute may have a leap second, since leap seconds happen in all zones simultaneously
      todSec = 1 -- :: Pico, type Pico = Fixed E12
    }

defaultUTCDate :: UTCTime
defaultUTCDate = localTimeToUTC utc defaultDate

fromFieldMoney ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Money
fromFieldMoney f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Money <$> fromField f mbValue

-- fromFieldCenti ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion Centi
-- fromFieldCenti f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case readMaybe (unpackChars value') of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')

-- fromFieldMinutes ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion Minutes
-- fromFieldMinutes f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just _ -> Minutes <$> fromField f mbValue

-- fromFieldHighPrecMeters ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion HighPrecMeters
-- fromFieldHighPrecMeters f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just _ -> HighPrecMeters <$> fromField f mbValue

-- fromFieldHighPrecMoney ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion HighPrecMoney
-- fromFieldHighPrecMoney f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just _ -> HighPrecMoney <$> fromField f mbValue

-- fromFieldSeconds ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion Seconds
-- fromFieldSeconds f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just _ -> Seconds <$> fromField f mbValue

-- instance FromField Minutes where
--   fromField = fromFieldMinutes

-- instance HasSqlValueSyntax be Integer => HasSqlValueSyntax be Centi where
--   sqlValueSyntax (MkFixed i) = sqlValueSyntax i

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

-- instance FromBackendRow Postgres Centesimal

-- instance FromField Centesimal where
--   fromField = fromFieldEnum

-- instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [Text] where
--   sqlValueSyntax x = sqlValueSyntax (V.fromList x)

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Text]

-- instance FromBackendRow Postgres [Text]

-- instance FromField [Text] where
--   fromField f mbValue = V.toList <$> fromField f mbValue

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centi

-- instance FromBackendRow Postgres Centi

-- instance IsString Centi where
--   fromString = show

-- instance HasSqlValueSyntax be Centi => HasSqlValueSyntax be Centesimal where
--   sqlValueSyntax = sqlValueSyntax . getCenti

-- instance HasSqlValueSyntax be Centesimal => HasSqlValueSyntax be HighPrecMeters where
--   sqlValueSyntax = sqlValueSyntax . getHighPrecMeters

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMeters

-- instance FromBackendRow Postgres HighPrecMeters

-- instance FromField HighPrecMeters where
--   fromField = fromFieldHighPrecMeters

-- instance IsString HighPrecMeters where
--   fromString = show

-- instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Meters where
--   sqlValueSyntax = sqlValueSyntax . getMeters

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

-- instance FromBackendRow Postgres Meters

-- instance FromField Meters where
--   fromField = fromFieldJSON

-- instance IsString Meters where
--   fromString = show

-- instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Seconds where
--   sqlValueSyntax = sqlValueSyntax . getSeconds

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

-- instance FromBackendRow Postgres Seconds

-- instance IsString Seconds where
--   fromString = show

-- instance FromField HighPrecMoney where
--   fromField = fromFieldHighPrecMoney

-- instance HasSqlValueSyntax be Rational => HasSqlValueSyntax be HighPrecMoney where
--   sqlValueSyntax = sqlValueSyntax . getHighPrecMoney

-- instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Rational where
--   sqlValueSyntax = sqlValueSyntax . (fromRational :: Rational -> Double)

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMoney

-- instance FromBackendRow Postgres HighPrecMoney

-- instance IsString HighPrecMoney where
--   fromString = show

-- instance FromField Seconds where
--   fromField = fromFieldSeconds

-- instance FromField DbHash where
--   fromField = fromFieldEnumDbHash

-- instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be DbHash where
--   sqlValueSyntax = sqlValueSyntax . unDbHash

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

-- instance FromBackendRow Postgres DbHash

-- instance IsString DbHash where
--   fromString = show

instance FromField Context.City where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Context.City where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Context.City

instance FromBackendRow Postgres Context.City

instance FromField Context.Country where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Context.Country where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Context.Country

instance FromBackendRow Postgres Context.Country

instance FromField DomainDO.DriverOfferStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DomainDO.DriverOfferStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainDO.DriverOfferStatus

instance FromBackendRow Postgres DomainDO.DriverOfferStatus

instance IsString DomainDO.DriverOfferStatus where
  fromString = show

deriving stock instance Ord DomainDO.DriverOfferStatus

-- instance FromField DomainDO.DriverOfferStatus where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be DomainDO.DriverOfferStatus where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainDO.DriverOfferStatus

-- instance FromBackendRow Postgres DomainDO.DriverOfferStatus

-- instance IsString DomainDO.DriverOfferStatus where
-- fromString = show

-- deriving stock instance Ord DomainDO.DriverOfferStatus

-- fromFieldJSON ::
--   (Typeable a, FromJSON a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldJSON f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' -> case A.decode $ fromStrict value' of
--     Just res -> pure res
--     Nothing -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Language where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance FromField Language => FromBackendRow Postgres Language

-- instance FromField Language where
--   fromField = fromFieldEnum

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Language

instance FromField Payment.Currency where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Payment.Currency where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Payment.Currency

instance FromBackendRow Postgres Payment.Currency

instance IsString Payment.Currency where
  fromString = show

deriving stock instance Ord Payment.Currency

instance FromField Payment.TransactionStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Payment.TransactionStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Payment.TransactionStatus

instance FromBackendRow Postgres Payment.TransactionStatus

instance IsString Payment.TransactionStatus where
  fromString = show

deriving stock instance Ord Payment.TransactionStatus

instance FromField VehVar.VehicleVariant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be VehVar.VehicleVariant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be VehVar.VehicleVariant

instance FromBackendRow Postgres VehVar.VehicleVariant

instance IsString VehVar.VehicleVariant where
  fromString = show

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case readMaybe (unpackChars value') of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')

-- fromFieldEnumDbHash ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion DbHash
-- fromFieldEnumDbHash f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' -> pure $ DbHash value'

-- getPoint :: (Double, Double) -> BQ.QGenExpr context Postgres s Point
-- getPoint (lat, lon) = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_SetSRID (ST_Point (" <> show lon <> " , " <> show lat <> "),4326)"))

-- containsPoint'' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
-- containsPoint'' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (" <> show lon <> " , " <> show lat <> ")")))

-- containsPoint' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
-- containsPoint' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (geom, ST_GeomFromText('POINT (" <> show lon <> " " <> show lat <> ")'))")))

-- buildRadiusWithin' :: Point -> (Double, Double) -> Int -> BQ.QGenExpr context Postgres s BQ.SqlBool
-- buildRadiusWithin' pnt (lat, lon) rad =
--   BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_DWithin(" <> show pnt <> " , " <> getPoint' <> " , " <> show rad <> ")"))
--   where
--     getPoint' = "(SRID=4326;POINT(" <> show lon <> " " <> show lat <> "))"

(<->.) :: Point -> Point -> BQ.QGenExpr context Postgres s Double
(<->.) p1 p2 = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ show p1 <> " <-> " <> show p2))

setFlagsInMeshConfig :: (L.MonadFlow m) => MeshConfig -> Text -> m MeshConfig
setFlagsInMeshConfig meshCfg modelName = do
  let isMeshEnabled = isKVEnabled modelName
      isKVHardKilled = isHardKillEnabled modelName
  pure $ meshCfg {meshEnabled = isMeshEnabled, kvHardKilled = isKVHardKilled}
  where
    isKVEnabled _ = False
    isHardKillEnabled _ = True

kvTables :: [Text]
kvTables = []

kvHardKilledTables :: [Text]
kvHardKilledTables = []

setMeshConfig :: Text -> MeshConfig
setMeshConfig modelTableName = meshConfig {meshEnabled = modelTableName `elem` kvTables, kvHardKilled = modelTableName `notElem` kvHardKilledTables}
