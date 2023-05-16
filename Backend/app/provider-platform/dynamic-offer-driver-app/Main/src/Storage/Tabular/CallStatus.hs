{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.CallStatus where

import qualified Domain.Types.CallStatus as Domain
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Ride (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CallStatusT sql=call_status
      id Text
      callId Text
      rideId RideTId
      dtmfNumberUsed Text Maybe
      status Call.CallStatus
      recordingUrl Text Maybe
      conversationDuration Int
      createdAt UTCTime
      UniqueCallStatusCallId callId
      Primary id
      deriving Generic
    |]

instance TEntityKey CallStatusT where
  type DomainKey CallStatusT = Id Domain.CallStatus
  fromKey (CallStatusTKey _id) = Id _id
  toKey (Id id) = CallStatusTKey id

instance FromTType CallStatusT Domain.CallStatus where
  fromTType CallStatusT {..} =
    return $
      Domain.CallStatus
        { id = Id id,
          rideId = fromKey rideId,
          ..
        }

instance ToTType CallStatusT Domain.CallStatus where
  toTType Domain.CallStatus {..} =
    CallStatusT
      { id = getId id,
        rideId = toKey rideId,
        ..
      }

-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}

-- module Storage.Tabular.CallStatus where

-- import qualified Data.Aeson as A
-- import Data.ByteString.Internal (ByteString, unpackChars)
-- import qualified Data.HashMap.Internal as HM
-- import qualified Data.Map.Strict as M
-- import Data.Serialize
-- import Data.Time
--   ( LocalTime (LocalTime),
--     TimeOfDay (TimeOfDay),
--     localDay,
--     localTimeOfDay,
--     localTimeToUTC,
--     todHour,
--     todMin,
--     todSec,
--     utc,
--   )
-- import qualified Data.Time as Time
-- import qualified Database.Beam as B
-- import Database.Beam.Backend
-- import Database.Beam.MySQL ()
-- import Database.Beam.Postgres
--   ( Postgres,
--     ResultError (ConversionFailed, UnexpectedNull),
--   )
-- import Database.PostgreSQL.Simple.FromField (FromField, fromField)
-- import qualified Database.PostgreSQL.Simple.FromField as DPSF
-- import qualified Domain.Types.CallStatus as Domain
-- import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
-- import GHC.Generics (Generic)
-- import qualified Kernel.External.Call.Interface.Types as Call
-- import Kernel.Prelude hiding (Generic)
-- import Kernel.Types.Common hiding (id)
-- import Lib.Utils
-- import Lib.UtilsTH
-- import Sequelize
-- import Storage.Tabular.Ride (RideTId)

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
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

-- -- instance FromField callId where
-- --   fromField = fromFieldEnum

-- -- instance HasSqlValueSyntax be String => HasSqlValueSyntax be callId where
-- --   sqlValueSyntax = autoSqlValueSyntax

-- -- instance BeamSqlBackend be => B.HasSqlEqualityCheck be callId

-- -- instance FromBackendRow Postgres callId

-- instance FromField Call.CallStatus where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Call.CallStatus where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Call.CallStatus

-- instance FromBackendRow Postgres Call.CallStatus

-- data CallStatusT f = CallStatusT
--   { id :: B.C f Text,
--     callId :: B.C f Text,
--     rideId :: B.C f Text,
--     dtmfNumberUsed :: B.C f (Maybe Text),
--     status :: B.C f Call.CallStatus,
--     recordingUrl :: B.C f (Maybe Text),
--     conversationDuration :: B.C f Int,
--     createdAt :: B.C f Time.UTCTime
--   }
--   deriving (Generic, B.Beamable)

-- instance B.Table CallStatusT where
--   data PrimaryKey CallStatusT f
--     = Id (B.C f Text)
--     deriving (Generic, B.Beamable)
--   primaryKey = Id . id

-- instance ModelMeta CallStatusT where
--   modelFieldModification = callStatusTMod
--   modelTableName = "call_status"
--   mkExprWithDefault _ = B.insertExpressions []

-- type CallStatus = CallStatusT Identity

-- instance FromJSON CallStatus where
--   parseJSON = A.genericParseJSON A.defaultOptions

-- instance ToJSON CallStatus where
--   toJSON = A.genericToJSON A.defaultOptions

-- deriving stock instance Ord Call.CallStatus

-- deriving stock instance Eq Call.CallStatus

-- deriving stock instance Show CallStatus

-- callStatusTMod :: CallStatusT (B.FieldModification (B.TableField CallStatusT))
-- callStatusTMod =
--   B.tableModification
--     { id = B.fieldNamed "id",
--       callId = B.fieldNamed "call_id",
--       rideId = B.fieldNamed "ride_id",
--       dtmfNumberUsed = B.fieldNamed "dtmf_number_used",
--       status = B.fieldNamed "status",
--       recordingUrl = B.fieldNamed "recording_url",
--       conversationDuration = B.fieldNamed "conversation_duration",
--       createdAt = B.fieldNamed "created_at"
--     }

-- instance IsString Call.CallStatus where
--   fromString = show

-- defaultCallStatus :: CallStatus
-- defaultCallStatus =
--   CallStatusT
--     { id = "", -- :: Int,
--       callId = "",
--       rideId = "",
--       dtmfNumberUsed = Nothing,
--       status = "",
--       recordingUrl = Nothing,
--       conversationDuration = 0,
--       createdAt = defaultUTCDate --localTimeToUTC utc defaultDate
--     }

-- psToHs :: HM.HashMap Text Text
-- psToHs = HM.empty

-- callStatusToHSModifiers :: M.Map Text (A.Value -> A.Value)
-- callStatusToHSModifiers =
--   M.empty

-- callStatusToPSModifiers :: M.Map Text (A.Value -> A.Value)
-- callStatusToPSModifiers =
--   M.empty

-- instance Serialize CallStatus where
--   put = error "undefined"
--   get = error "undefined"

-- $(enableKVPG ''CallStatusT ['id] [])
