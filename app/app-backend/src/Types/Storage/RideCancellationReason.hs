{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideCancellationReason where

import Beckn.Types.Core.Taxi.Common.CancellationSource (CancellationSource)
import Beckn.Types.Id
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.CancellationReason (CancellationReasonCode, CancellationStage)
import Types.Storage.RideBooking (RideBooking)

data RideCancellationReasonT f = RideCancellationReason
  { rideBookingId :: B.C f (Id RideBooking),
    source :: B.C f CancellationSource,
    reasonCode :: B.C f (Maybe CancellationReasonCode),
    reasonStage :: B.C f (Maybe CancellationStage),
    additionalInfo :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type RideCancellationReason = RideCancellationReasonT Identity

type RideCancellationReasonPrimaryKey = B.PrimaryKey RideCancellationReasonT Identity

instance B.Table RideCancellationReasonT where
  data PrimaryKey RideCancellationReasonT f = RideCancellationReasonPrimaryKey (B.C f (Id RideBooking))
    deriving (Generic, B.Beamable)
  primaryKey = RideCancellationReasonPrimaryKey . rideBookingId

instance ToJSON RideCancellationReason

instance FromJSON RideCancellationReason

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideCancellationReasonT)
fieldEMod =
  B.setEntityName "ride_cancellation_reason"
    <> B.modifyTableFields
      B.tableModification
        { rideBookingId = "ride_booking_id",
          reasonCode = "reason_code",
          reasonStage = "reason_stage",
          additionalInfo = "additional_info"
        }
