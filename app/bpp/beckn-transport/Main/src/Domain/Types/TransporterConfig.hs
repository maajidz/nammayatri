{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.TransporterConfig where

import Data.Time (UTCTime)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import EulerHS.Prelude hiding (id)
import Kernel.External.FCM.Types (FCMConfig)
import Kernel.Types.Common
import Kernel.Types.Id

data TransporterConfigD u = TransporterConfig
  { merchantId :: Id Merchant,
    pickupLocThreshold :: Maybe Meters,
    dropLocThreshold :: Maybe Meters,
    rideTravelledDistThresholdWhenPickupOrDestIsDiff :: Maybe Meters,
    rideTravelledDistThresholdWhenPickupAndDestIsSame :: Maybe Meters,
    rideTimeEstimatedThreshold :: Maybe Seconds,
    waitingTimeEstimatedThreshold :: Maybe Seconds,
    availabilityTimeWeightage :: Int,
    acceptanceRatioWeightage :: Int,
    cancellationRatioWeightage :: Int,
    maxRadius :: Meters,
    minRadius :: Meters,
    radiusStepSize :: Meters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    fcmConfig :: FCMConfig
  }
  deriving (Generic, Show)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)

data TransporterParameter

newtype ConfigKey = ConfigKey
  { getConfigKey :: Text
  }
  deriving stock (Generic)
  deriving newtype (Show, Read, FromJSON, ToJSON)
