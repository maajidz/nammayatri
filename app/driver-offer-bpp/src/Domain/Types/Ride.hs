{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Ride where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Person as DPers
import qualified Domain.Types.RideBooking as DRB
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.Vehicle.Variant as DVeh
import EulerHS.Prelude hiding (id)
import Servant.API
import Utils.Common

data RideStatus
  = INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Ride = Ride
  { id :: Id Ride,
    bookingId :: Id DRB.RideBooking,
    shortId :: ShortId Ride,
    status :: RideStatus,
    driverId :: Id DPers.Person,
    vehicleId :: Id DVeh.Vehicle,
    otp :: Text,
    trackingUrl :: Text,
    fare :: Maybe Amount,
    traveledDistance :: HighPrecMeters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    bookingId :: Id DRB.RideBooking,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: DVeh.Variant,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Amount,
    actualRideDistance :: HighPrecMeters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

makeRideAPIEntity :: Ride -> DPers.DecryptedPerson -> DVeh.Vehicle -> RideAPIEntity
makeRideAPIEntity ride driver vehicle =
  RideAPIEntity
    { id = ride.id,
      bookingId = ride.bookingId,
      shortRideId = ride.shortId,
      status = ride.status,
      driverName = driver.firstName,
      driverNumber = driver.mobileCountryCode <> driver.mobileNumber,
      vehicleNumber = vehicle.registrationNo,
      vehicleColor = vehicle.color,
      vehicleVariant = vehicle.variant,
      vehicleModel = vehicle.model,
      computedFare = ride.fare,
      actualRideDistance = ride.traveledDistance,
      createdAt = ride.createdAt,
      updatedAt = ride.updatedAt
    }
