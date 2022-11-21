module Domain.Action.UI.Booking
  ( BookingListRes (..),
    GetRideInfoRes (..),
    RideInfo (..),
    SetDriverAcceptanceReq (..),
    SetDriverAcceptanceRes,
    NotificationStatus (..),
    DriverResponse (..),
    bookingStatus,
    bookingList,
    bookingCancel,
    getRideInfo,
    setDriverAcceptance,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.OpenApi (ToSchema (..))
import Domain.Types.AllocationEvent
import qualified Domain.Types.AllocationEvent as AllocationEvent
import qualified Domain.Types.Booking as SRB
import Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.Person as SP
import Domain.Types.RideRequest
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude hiding (id)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.AllocationEvent as AllocationEvent
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RideRequest as RideRequest
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

data RideInfo = RideInfo
  { bookingId :: Id SRB.Booking,
    pickupLoc :: BookingLocationAPIEntity,
    dropLoc :: Maybe BookingLocationAPIEntity,
    etaForPickupLoc :: Minutes,
    distanceToPickupLoc :: Meters,
    notificationExpiryTime :: UTCTime,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

newtype SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { response :: NotificationStatus
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type SetDriverAcceptanceRes = APISuccess

bookingStatus :: (CacheFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Id SRB.Booking -> m SRB.BookingAPIEntity
bookingStatus bookingId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  SRB.buildBookingAPIEntity booking

bookingList ::
  (CacheFlow m r, EsqDBReplicaFlow m r, EncFlow m r) =>
  SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe SRB.BookingStatus ->
  m BookingListRes
bookingList person mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  let Just merchantId = person.merchantId
  rbList <- QRB.findAllByMerchant merchantId mbLimit mbOffset mbOnlyActive mbBookingStatus
  BookingListRes <$> traverse SRB.buildBookingAPIEntity rbList

bookingCancel ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id SRB.Booking ->
  SP.Person ->
  m APISuccess
bookingCancel bookingId admin = do
  let Just merchantId = admin.merchantId
  org <-
    QM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  rideReq <- buildRideReq (org.subscriberId)
  Esq.runTransaction $ RideRequest.create rideReq
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> bookingCancel : ") (show rideReq)
  return Success
  where
    buildRideReq subscriberId = do
      guid <- generateGUID
      now <- getCurrentTime
      pure
        SRideRequest.RideRequest
          { id = Id guid,
            bookingId = bookingId,
            subscriberId = subscriberId,
            createdAt = now,
            _type = SRideRequest.CANCELLATION,
            info = Nothing
          }

getRideInfo ::
  (EncFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, CoreMetrics m) => Id SRB.Booking -> Id SP.Person -> m GetRideInfoRes
getRideInfo bookingId personId = do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId bookingId
  case mbNotification of
    Nothing -> return $ GetRideInfoRes Nothing
    Just notification -> do
      let notificationExpiryTime = notification.expiresAt
      booking <- QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
      driver <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      driverLocation <-
        QDrLoc.findById driver.id
          >>= fromMaybeM LocationNotFound
      let fromLocation = booking.fromLocation
      let toLocation = case booking.bookingDetails of
            SRB.OneWayDetails details -> Just details.toLocation
            SRB.RentalDetails _ -> Nothing
      distanceDuration <-
        MapSearch.getDistance booking.providerId $
          MapSearch.GetDistanceReq
            { origin = driverLocation,
              destination = fromLocation,
              travelMode = Just MapSearch.CAR
            }
      return $
        GetRideInfoRes $
          Just $
            RideInfo
              { bookingId = booking.id,
                pickupLoc = DBLoc.makeBookingLocationAPIEntity fromLocation,
                dropLoc = DBLoc.makeBookingLocationAPIEntity <$> toLocation,
                etaForPickupLoc = secondsToMinutes $ distanceDuration.duration,
                distanceToPickupLoc = distanceDuration.distance,
                notificationExpiryTime = notificationExpiryTime,
                estimatedFare = booking.estimatedFare,
                discount = booking.discount,
                estimatedTotalFare = booking.estimatedTotalFare
              }
  where
    driverId = cast personId

responseToEventType :: NotificationStatus -> AllocationEventType
responseToEventType ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SRB.Booking -> Id SP.Person -> SetDriverAcceptanceReq -> m SetDriverAcceptanceRes
setDriverAcceptance bookingId personId req = do
  currentTime <- getCurrentTime
  logTagInfo "setDriverAcceptance" logMessage
  booking <-
    QRB.findById bookingId
      >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  merchant <-
    QM.findById booking.providerId
      >>= fromMaybeM (MerchantDoesNotExist booking.providerId.getId)
  guid <- generateGUID
  let driverResponse =
        DriverResponse {driverId = driverId, status = req.response}
  let rideRequest =
        RideRequest
          { id = Id guid,
            bookingId = bookingId,
            subscriberId = merchant.subscriberId,
            createdAt = currentTime,
            _type = DRIVER_RESPONSE,
            info = Just driverResponse
          }
  Esq.runTransaction $ do
    RideRequest.create rideRequest
    AllocationEvent.logAllocationEvent
      (responseToEventType response)
      bookingId
      (Just driverId)
  pure Success
  where
    response = req.response
    driverId = cast personId
    logMessage =
      "beckn:" <> bookingId.getId <> ":"
        <> getId driverId
        <> ":response"
        <> " "
        <> show response
