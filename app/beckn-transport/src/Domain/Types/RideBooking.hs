module Domain.Types.RideBooking where

import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.BookingLocation as DLoc
import qualified Domain.Types.FareProduct as SFP
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Servant.API

data RideBookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideBookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideBookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data RideBooking = RideBooking
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    providerId :: Id DOrg.Organization,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocationId :: Id DLoc.BookingLocation,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    reallocationsCount :: Int,
    rideBookingDetails :: RideBookingDetails,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

data RideBookingDetails = OneWayDetails OneWayRideBookingDetails | RentalDetails RentalRideBookingDetails
  deriving (Generic, Eq)

data OneWayRideBookingDetails = OneWayRideBookingDetails
  { toLocationId :: Id DLoc.BookingLocation,
    estimatedDistance :: HighPrecMeters
  }
  deriving (Eq)

newtype RentalRideBookingDetails = RentalRideBookingDetails
  { rentalFarePolicyId :: Id DRentalFP.RentalFarePolicy
  }
  deriving (Eq)

mkRentalRideBookingDetails :: Id DRentalFP.RentalFarePolicy -> RideBookingDetails
mkRentalRideBookingDetails rentalFarePolicyId = RentalDetails $ RentalRideBookingDetails {..}

getFareProductType :: RideBookingDetails -> SFP.FareProductType
getFareProductType = \case
  OneWayDetails _ -> SFP.ONE_WAY
  RentalDetails _ -> SFP.RENTAL
