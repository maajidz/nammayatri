module Services.Allocation.Internal where

import App.BackgroundTaskManager.Types (DriverAllocationConfig)
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import Product.BecknProvider.Cancel
import qualified Product.BecknProvider.Confirm as Confirm
import Services.Allocation.Allocation as Alloc
import Storage.Queries.AllocationEvent (logAllocationEvent)
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.NotificationStatus as QNS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideCancellationReason as QRCR
import qualified Storage.Queries.RideRequest as QRR
import qualified Storage.Queries.Vehicle as QVeh
import Types.App
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.AllocationEvent (AllocationEventType)
import qualified Types.Storage.DriverInformation as SDriverInfo
import qualified Types.Storage.NotificationStatus as SNS
import Types.Storage.Organization
import qualified Types.Storage.Ride as SRide
import Types.Storage.RideBooking (RideBooking)
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.RideRequest as SRR
import Utils.Common
import Utils.Notifications
import qualified Utils.Notifications as Notify

getDriverSortMode :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m SortMode
getDriverSortMode = asks (.driverAllocationConfig.defaultSortMode)

getConfiguredNotificationTime :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m Seconds
getConfiguredNotificationTime = asks (.driverAllocationConfig.driverNotificationExpiry)

getConfiguredAllocationTime :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m Seconds
getConfiguredAllocationTime = asks (.driverAllocationConfig.rideAllocationExpiry)

getDriverPool :: (DBFlow m r, HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]) => Id RideBooking -> m [Id Driver]
getDriverPool = Confirm.getDriverPool

getDriverBatchSize :: HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m Int
getDriverBatchSize = asks (.driverAllocationConfig.driverBatchSize)

getRequests :: DBFlow m r => ShortId Organization -> Integer -> m [RideRequest]
getRequests shortOrgId numRequests = do
  allRequests <- QRR.fetchOldest shortOrgId numRequests
  let (errors, requests) =
        partitionEithers $ map toRideRequest allRequests
  traverse_ logError errors
  pure requests

assignDriver ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id RideBooking ->
  Id Driver ->
  m ()
assignDriver rideBookingId driverId = do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingDoesNotExist
  driver <-
    QP.findPersonById (cast driverId)
      >>= fromMaybeM PersonDoesNotExist
  ride <- buildRide rideBooking driver
  DB.runSqlDBTransaction $ do
    QDI.updateOnRide (cast driver.id) True
    QRB.updateStatus rideBookingId SRB.TRIP_ASSIGNED
    QRide.create ride

  fork "assignDriver - Notify BAP" $ do
    uRideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingNotFound
    BP.sendRideAssignedUpdateToBAP uRideBooking ride
    Notify.notifyDriver notificationType notificationTitle (message uRideBooking) driver.id driver.deviceToken
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message ride =
      unwords
        [ "You have been assigned a ride for",
          showTimeIst ride.startTime <> ".",
          "Check the app for more details."
        ]
    buildRide rideBooking driver = do
      vehicleId <-
        driver.udf1
          & fromMaybeM (PersonFieldNotPresent "udf1 - vehicle")
          <&> Id
      vehicle <-
        QVeh.findVehicleById vehicleId
          >>= fromMaybeM VehicleNotFound
      guid <- generateGUID
      shortId <- generateShortId
      otp <- generateOTPCode
      now <- getCurrentTime
      return
        SRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            shortId = shortId,
            status = SRide.NEW,
            driverId = driver.id,
            vehicleId = vehicle.id,
            otp = otp,
            trackingUrl = "UNKNOWN", -- TODO: Fill this field
            fare = Nothing,
            totalFare = Nothing,
            traveledDistance = 0,
            chargeableDistance = Nothing,
            createdAt = now,
            updatedAt = now
          }

toRideRequest :: SRR.RideRequest -> Either Text Alloc.RideRequest
toRideRequest req =
  case eRequestData of
    Right requestData ->
      Right
        Alloc.RideRequest
          { requestId = req.id,
            rideBookingId = req.rideBookingId,
            requestData = requestData
          }
    Left err -> Left err
  where
    eRequestData = case req._type of
      SRR.ALLOCATION -> Right Alloc.Allocation
      SRR.CANCELLATION -> Right Alloc.Cancellation
      SRR.DRIVER_RESPONSE ->
        case req.info >>= decodeFromText of
          Just driverResponse -> Right $ DriverResponse driverResponse
          Nothing -> Left $ "Error decoding driver response: " <> show req.info

getCurrentNotifications ::
  DBFlow m r =>
  Id RideBooking ->
  m [CurrentNotification]
getCurrentNotifications rideBookingId = do
  notificationStatuses <- QNS.findActiveNotificationByRideId rideBookingId
  pure $ map buildCurrentNotification notificationStatuses
  where
    buildCurrentNotification notificationStatus =
      Alloc.CurrentNotification
        (notificationStatus.driverId)
        (notificationStatus.expiresAt)

cleanupOldNotifications :: DBFlow m r => m Int
cleanupOldNotifications = QNS.cleanupOldNotifications

sendNewRideNotifications ::
  ( DBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id RideBooking ->
  NonEmpty (Id Driver) ->
  m ()
sendNewRideNotifications rideBookingId = traverse_ sendNewRideNotification
  where
    sendNewRideNotification driverId = do
      rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingNotFound
      person <-
        QP.findPersonById (cast driverId)
          >>= fromMaybeM PersonNotFound
      notifyDriverNewAllocation rideBooking.id person.id person.deviceToken

sendRideNotAssignedNotification ::
  ( DBFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id RideBooking ->
  Id Driver ->
  m ()
sendRideNotAssignedNotification rideBookingId driverId = do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingNotFound
  person <-
    QP.findPersonById (cast driverId)
      >>= fromMaybeM PersonNotFound
  notifyRideNotAssigned rideBooking.id person.id person.deviceToken

updateNotificationStatuses :: DBFlow m r => Id RideBooking -> NotificationStatus -> NonEmpty (Id Driver) -> m ()
updateNotificationStatuses rideBookingId status driverIds = do
  let storageStatus = allocNotifStatusToStorageStatus status
  QNS.updateStatus rideBookingId storageStatus $ toList driverIds

resetLastRejectionTimes :: DBFlow m r => NonEmpty (Id Driver) -> m ()
resetLastRejectionTimes driverIds = QDS.updateIdleTimesFlow $ toList driverIds

getAttemptedDrivers :: DBFlow m r => Id RideBooking -> m [Id Driver]
getAttemptedDrivers rideBookingId =
  QNS.fetchRefusedNotificationsByRideId rideBookingId <&> map (.driverId)

getDriversWithNotification :: DBFlow m r => m [Id Driver]
getDriversWithNotification = QNS.fetchActiveNotifications <&> fmap (.driverId)

getTopDriversByIdleTime :: DBFlow m r => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime = QDS.getTopDriversByIdleTime

checkAvailability :: DBFlow m r => NonEmpty (Id Driver) -> m [Id Driver]
checkAvailability driverIds = do
  driversInfo <- QDriverInfo.fetchAllAvailableByIds $ toList driverIds
  pure $ map (cast . SDriverInfo.driverId) driversInfo

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id SRB.RideBooking ->
  SRCR.RideCancellationReason ->
  m ()
cancelRide rideBookingId reason = do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingNotFound
  mbRide <- QRide.findByRBId rideBookingId
  DB.runSqlDBTransaction $ do
    QRCR.create reason
    QRB.updateStatus rideBooking.id SRB.CANCELLED
    whenJust mbRide $ \ride -> do
      QRide.updateStatus ride.id SRide.CANCELLED
      QDriverInfo.updateOnRide (cast ride.driverId) False
  logTagInfo ("rideBookingId-" <> getId rideBookingId) ("Cancellation reason " <> show reason.source)
  notifyBAPOnCancel rideBooking reason
  whenJust mbRide $ \ride ->
    notifyDriverOnCancel rideBooking ride reason

cleanupNotifications :: DBFlow m r => Id RideBooking -> m ()
cleanupNotifications = QNS.cleanupNotifications

removeRequest :: DBFlow m r => Id SRR.RideRequest -> m ()
removeRequest = QRR.removeRequest

allocNotifStatusToStorageStatus ::
  Alloc.NotificationStatus ->
  SNS.AnswerStatus
allocNotifStatusToStorageStatus = \case
  Alloc.Notified -> SNS.NOTIFIED
  Alloc.Rejected -> SNS.REJECTED
  Alloc.Ignored -> SNS.IGNORED

addAllocationRequest :: DBFlow m r => ShortId Organization -> Id RideBooking -> m ()
addAllocationRequest shortOrgId rideBookingId = do
  guid <- generateGUID
  currTime <- getCurrentTime
  let rideRequest =
        SRR.RideRequest
          { id = Id guid,
            rideBookingId = rideBookingId,
            shortOrgId = shortOrgId,
            createdAt = currTime,
            _type = SRR.ALLOCATION,
            info = Nothing
          }
  QRR.createFlow rideRequest

addNotificationStatuses ::
  DBFlow m r =>
  Id RideBooking ->
  NonEmpty (Id Driver) ->
  UTCTime ->
  m ()
addNotificationStatuses rideBookingId driverIds expiryTime = do
  notificationStatuses <- traverse createNotificationStatus driverIds
  QNS.createMany $ toList notificationStatuses
  where
    createNotificationStatus driverId = do
      uuid <- generateGUID
      pure $
        SNS.NotificationStatus
          { id = Id uuid,
            rideBookingId = rideBookingId,
            driverId = driverId,
            status = SNS.NOTIFIED,
            expiresAt = expiryTime
          }

addAvailableDriver :: SDriverInfo.DriverInformation -> [Id Driver] -> [Id Driver]
addAvailableDriver driverInfo availableDriversIds =
  if driverInfo.active && not (driverInfo.onRide)
    then cast (driverInfo.driverId) : availableDriversIds
    else availableDriversIds

logEvent :: DBFlow m r => AllocationEventType -> Id RideBooking -> m ()
logEvent eventType rideBookingId = logAllocationEvent eventType rideBookingId Nothing

logDriverEvents :: DBFlow m r => AllocationEventType -> Id RideBooking -> NonEmpty (Id Driver) -> m ()
logDriverEvents eventType rideBookingId = traverse_ logDriverEvent
  where
    logDriverEvent driver = logAllocationEvent eventType rideBookingId $ Just driver

-- TODO: We don't need RideInfo anymore, we can just use RideBooking directly. Remove this.
getRideInfo :: DBFlow m r => Id RideBooking -> m RideInfo
getRideInfo rideBookingId = do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingNotFound
  rideStatus <- castToRideStatus $ rideBooking.status
  pure
    RideInfo
      { rideBookingId = rideBookingId,
        rideStatus = rideStatus,
        orderTime = OrderTime $ rideBooking.createdAt
      }
  where
    castToRideStatus = \case
      SRB.CONFIRMED -> pure Confirmed
      SRB.TRIP_ASSIGNED -> pure Assigned
      SRB.COMPLETED -> pure Completed
      SRB.CANCELLED -> pure Cancelled
