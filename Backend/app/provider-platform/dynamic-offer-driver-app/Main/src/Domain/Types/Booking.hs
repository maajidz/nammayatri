{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Data.Time.Clock.POSIX as Time
import Domain.Types.FareParameters (FareParameters)
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.LocationMapping as DLocationMapping
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle.Variant as DVeh
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Servant.API

data BookingStatus
  = NEW
  | TRIP_ASSIGNED
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Booking = Booking
  { id :: Id Booking,
    transactionId :: Text,
    quoteId :: Text,
    status :: BookingStatus,
    bookingType :: BookingType,
    specialZoneOtpCode :: Maybe Text,
    specialLocationTag :: Maybe Text,
    area :: Maybe FareProductD.Area,
    providerId :: Id DM.Merchant, -- FIXME merchantId
    primaryExophone :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Maybe Context.City,
    bapCountry :: Maybe Context.Country,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocation :: DLocation.Location,
    toLocation :: [DLocation.Location],
    vehicleVariant :: DVeh.Variant,
    estimatedDistance :: Meters,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    estimatedFare :: Money,
    estimatedDuration :: Seconds,
    fareParams :: FareParameters,
    riderName :: Maybe Text,
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

data BookingType = SpecialZoneBooking | NormalBooking
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

locationMappingMakerForBooking :: (MonadFlow m) => Booking -> m [DLocationMapping.LocationMapping]
locationMappingMakerForBooking booking = do
  let bookingWithIndexes = zip ([1 ..] :: [Int]) booking.toLocation
  toLocationMappers <- mapM (locationMappingMakerForBookingInstanceMaker booking) bookingWithIndexes
  fromLocationMapping <- locationMappingMakerForBookingInstanceMaker booking (0, booking.fromLocation)
  return $ fromLocationMapping : toLocationMappers

locationMappingMakerForBookingInstanceMaker :: (MonadFlow m) => Booking -> (Int, DLocation.Location) -> m DLocationMapping.LocationMapping
locationMappingMakerForBookingInstanceMaker Booking {..} location = do
  locationMappingId <- generateGUID
  let getIntEpochTime = round `fmap` Time.getPOSIXTime
  epochVersion <- liftIO getIntEpochTime
  let epochVersionLast5Digits = epochVersion `mod` 100000 :: Integer
  let locationMapping =
        DLocationMapping.LocationMapping
          { id = Id locationMappingId,
            tag = DLocationMapping.Booking,
            order = fst location,
            version = show epochVersionLast5Digits,
            tagId = getId id,
            location = snd location,
            ..
          }
  return locationMapping
