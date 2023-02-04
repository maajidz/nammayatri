module Domain.Action.Dashboard.Ride
  ( rideList,
    rideInfo,
    rideSync,
  )
where

import Beckn.External.Encryption (decrypt, getDbHash)
import Beckn.External.Maps.HasCoordinates
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Transactionable (runInReplica)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Ride as Common
import Data.Coerce (coerce)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.BookingCancellationReason as DBCReason
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import qualified SharedLogic.CallBAP as CallBAP
import SharedLogic.Transporter (findMerchantByShortId)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.DriverQuote as DQ
import qualified Storage.Queries.FareParameters as QFareParams
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.RiderDetails as QRiderDetails
import Tools.Error

---------------------------------------------------------------------
rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbReqShortRideId mbCustomerPhone mbDriverPhone = do
  merchant <- findMerchantByShortId merchantShortId
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit -- TODO move to common code
      offset = fromMaybe 0 mbOffset
  let mbShortRideId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) <$> mbReqShortRideId
  mbCustomerPhoneDBHash <- getDbHash `traverse` mbCustomerPhone
  mbDriverPhoneDBHash <- getDbHash `traverse` mbDriverPhone
  now <- getCurrentTime
  rideItems <- runInReplica $ QRide.findAllRideItems merchant.id limit offset mbBookingStatus mbShortRideId mbCustomerPhoneDBHash mbDriverPhoneDBHash now
  rideListItems <- traverse buildRideListItem rideItems
  let count = length rideListItems
  -- should we consider filters in totalCount, e.g. count all canceled rides?
  totalCount <- runInReplica $ QRide.countRides merchant.id
  let summary = Common.Summary {totalCount, count}
  pure Common.RideListRes {totalItems = count, summary, rides = rideListItems}
  where
    maxLimit = 20
    defaultLimit = 10

buildRideListItem :: EncFlow m r => QRide.RideItem -> m Common.RideListItem
buildRideListItem QRide.RideItem {..} = do
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  driverPhoneNo <- mapM decrypt rideDetails.driverNumber
  pure
    Common.RideListItem
      { rideId = cast @DRide.Ride @Common.Ride rideDetails.id,
        rideShortId = coerce @(ShortId DRide.Ride) @(ShortId Common.Ride) rideShortId,
        customerName,
        customerPhoneNo,
        driverName = rideDetails.driverName,
        driverPhoneNo,
        vehicleNo = rideDetails.vehicleNumber,
        bookingStatus
      }

---------------------------------------------------------------------
rideInfo :: ShortId DM.Merchant -> Id Common.Ride -> Flow Common.RideInfoRes
rideInfo merchantShortId reqRideId = do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideDetails <- runInReplica $ QRideDetails.findById rideId >>= fromMaybeM (RideNotFound rideId.getId) -- FIXME RideDetailsNotFound
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound rideId.getId)
  quote <- runInReplica $ DQ.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)
  let driverId = ride.driverId

  -- merchant access checking
  unless (merchant.id == booking.providerId) $ throwError (RideDoesNotExist rideId.getId)

  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound rideId.getId)
  driverLocation <- runInReplica $ QDrLoc.findById driverId >>= fromMaybeM LocationNotFound

  mbBCReason <-
    if ride.status == DRide.CANCELLED
      then runInReplica $ QBCReason.findByRideId rideId -- it can be Nothing if cancelled by user
      else pure Nothing
  driverInitiatedCallCount <- runInReplica $ QCallStatus.countCallsByRideId rideId
  let cancellationReason =
        (coerce @DCReason.CancellationReasonCode @Common.CancellationReasonCode <$>) . join $ mbBCReason <&> (.reasonCode)
  let cancelledBy = castCancellationSource <$> (mbBCReason <&> (.source))
  let cancelledTime = case ride.status of
        DRide.CANCELLED -> Just ride.updatedAt
        _ -> Nothing
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  driverPhoneNo <- mapM decrypt rideDetails.driverNumber
  now <- getCurrentTime
  pure
    Common.RideInfoRes
      { rideId = cast @DRide.Ride @Common.Ride ride.id,
        customerName = booking.riderName,
        customerPhoneNo,
        rideOtp = ride.otp,
        customerPickupLocation = mkLocationAPIEntity booking.fromLocation,
        customerDropLocation = Just $ mkLocationAPIEntity booking.toLocation,
        actualDropLocation = ride.tripEndPos,
        driverId = cast @DP.Person @Common.Driver driverId,
        driverName = rideDetails.driverName,
        driverPhoneNo,
        vehicleNo = rideDetails.vehicleNumber,
        driverStartLocation = ride.tripStartPos,
        driverCurrentLocation = getCoordinates driverLocation,
        rideBookingTime = booking.createdAt,
        estimatedDriverArrivalTime = realToFrac quote.durationToPickup `addUTCTime` ride.createdAt,
        actualDriverArrivalTime = ride.driverArrivalTime,
        rideStartTime = ride.tripStartTime,
        rideEndTime = ride.tripEndTime,
        rideDistanceEstimated = Just booking.estimatedDistance,
        rideDistanceActual = roundToIntegral ride.traveledDistance,
        estimatedRideDuration = Just $ secondsToMinutes booking.estimatedDuration,
        estimatedFare = booking.estimatedFare,
        actualFare = ride.fare,
        pickupDuration = timeDiffInMinutes <$> ride.tripStartTime <*> (Just ride.createdAt),
        rideDuration = timeDiffInMinutes <$> ride.tripEndTime <*> ride.tripStartTime,
        bookingStatus = mkBookingStatus ride now,
        cancelledTime,
        cancelledBy,
        cancellationReason,
        driverInitiatedCallCount,
        bookingToRideStartDuration = timeDiffInMinutes <$> ride.tripStartTime <*> (Just booking.createdAt)
      }

mkLocationAPIEntity :: DBLoc.BookingLocation -> Common.LocationAPIEntity
mkLocationAPIEntity DBLoc.BookingLocation {..} = do
  let DBLoc.LocationAddress {..} = address
  Common.LocationAPIEntity {..}

castCancellationSource :: DBCReason.CancellationSource -> Common.CancellationSource
castCancellationSource = \case
  DBCReason.ByUser -> Common.ByUser
  DBCReason.ByDriver -> Common.ByDriver
  DBCReason.ByMerchant -> Common.ByMerchant
  DBCReason.ByAllocator -> Common.ByAllocator
  DBCReason.ByApplication -> Common.ByApplication

timeDiffInMinutes :: UTCTime -> UTCTime -> Minutes
timeDiffInMinutes t1 = secondsToMinutes . nominalDiffTimeToSeconds . diffUTCTime t1

-- ride considered as ONGOING_6HRS if ride.status = INPROGRESS, but somehow ride.tripStartTime = Nothing
mkBookingStatus :: DRide.Ride -> UTCTime -> Common.BookingStatus
mkBookingStatus ride now = do
  let sixHours = secondsToNominalDiffTime $ Seconds 21600
  let ongoing6HrsCond = maybe True (\tripStartTime -> diffUTCTime now tripStartTime >= sixHours) ride.tripStartTime
  case ride.status of
    DRide.NEW -> Common.UPCOMING
    DRide.INPROGRESS | not ongoing6HrsCond -> Common.ONGOING
    DRide.INPROGRESS -> Common.ONGOING_6HRS
    DRide.COMPLETED -> Common.COMPLETED
    DRide.CANCELLED -> Common.CANCELLED

---------------------------------------------------------------------
rideSync :: ShortId DM.Merchant -> Id Common.Ride -> Flow Common.RideSyncRes
rideSync merchantShortId reqRideId = do
  merchant <- findMerchantByShortId merchantShortId
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound rideId.getId)

  -- merchant access checking
  unless (merchant.id == booking.providerId) $ throwError (RideDoesNotExist rideId.getId)

  logTagInfo "dashboard -> syncRide : " $ show rideId <> "; status: " <> show ride.status

  case ride.status of
    DRide.NEW -> pure $ Common.RideSyncRes Common.RIDE_NEW "Nothing to sync, ignoring"
    DRide.INPROGRESS -> pure $ Common.RideSyncRes Common.RIDE_INPROGRESS "Nothing to sync, ignoring"
    DRide.COMPLETED -> syncCompletedRide ride booking
    DRide.CANCELLED -> syncCancelledRide rideId booking merchant

syncCancelledRide :: Id DRide.Ride -> DBooking.Booking -> DM.Merchant -> Flow Common.RideSyncRes
syncCancelledRide rideId booking merchant = do
  mbBookingCReason <- runInReplica $ QBCReason.findByRideId rideId
  -- rides cancelled by bap no need to sync, and have mbBookingCReason = Nothing
  case mbBookingCReason of
    Nothing -> pure $ Common.RideSyncRes Common.RIDE_CANCELLED "No cancellation reason found for ride. Nothing to sync, ignoring"
    Just bookingCReason -> do
      case bookingCReason.source of
        DBCReason.ByUser -> pure $ Common.RideSyncRes Common.RIDE_CANCELLED "Ride canceled by customer. Nothing to sync, ignoring"
        source -> do
          -- do we need fork here?
          -- fork "cancelBooking" $ do
          CallBAP.sendBookingCancelledUpdateToBAP booking merchant source
          pure $ Common.RideSyncRes Common.RIDE_CANCELLED "Success. Sent booking cancellation update to bap"

syncCompletedRide :: DRide.Ride -> DBooking.Booking -> Flow Common.RideSyncRes
syncCompletedRide ride booking = do
  -- for old rides fareParametersId = Nothing, so throw E400
  fareParametersId <- ride.fareParametersId & fromMaybeM (FareParametersDoNotExist ride.id.getId)
  fareParameters <- runInReplica $ QFareParams.findById fareParametersId >>= fromMaybeM (FareParametersNotFound fareParametersId.getId)
  CallBAP.sendRideCompletedUpdateToBAP booking ride fareParameters
  pure $ Common.RideSyncRes Common.RIDE_COMPLETED "Success. Sent ride complete update to bap"
