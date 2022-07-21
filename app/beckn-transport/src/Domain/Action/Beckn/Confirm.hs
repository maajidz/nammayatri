module Domain.Action.Beckn.Confirm
  ( confirm,
    DConfirmReq (..),
    DConfirmRes (..),
    ConfirmResBDetails (..),
  )
where

import App.Scheduler
import Beckn.External.Encryption (encrypt)
import Beckn.External.GoogleMaps.Types
import Beckn.Prelude
import Beckn.Scheduler
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import Beckn.Types.Registry (Subscriber (..))
import qualified Data.Text as T
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingLocation as SBL
import qualified Domain.Types.BusinessEvent as SB
import Domain.Types.DiscountTransaction
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.RideRequest as RideRequest
import qualified Domain.Types.RiderDetails as SRD
import qualified Product.BecknProvider.BP as BP
import qualified SharedLogic.DriverPool as DrPool
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingLocation as QBL
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DiscountTransaction as QDiscTransaction
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as QRFP
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Metrics
import Types.Error
import Utils.Common

data DConfirmReq = DConfirmReq
  { bookingId :: Id SRB.Booking,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: SBL.LocationAddress,
    toAddress :: Maybe SBL.LocationAddress
  }

data DConfirmRes = DConfirmRes
  { booking :: SRB.Booking,
    bDetails :: ConfirmResBDetails,
    fromLocation :: SBL.BookingLocation,
    toLocation :: Maybe SBL.BookingLocation,
    riderDetails :: SRD.RiderDetails,
    transporter :: DOrg.Organization
  }

data ConfirmResBDetails
  = OneWayDetails
  | RentalDetails
      { baseDuration :: Hours,
        baseDistance :: Kilometers
      }

confirm ::
  ( EncFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["schedulingReserveTime" ::: Seconds],
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Id Organization.Organization ->
  Subscriber ->
  DConfirmReq ->
  m DConfirmRes
confirm transporterId subscriber req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let transporterId' = booking.providerId
  transporter <-
    Organization.findById transporterId'
      >>= fromMaybeM (OrgNotFound transporterId'.getId)
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let bapOrgId = booking.bapId
  unless (subscriber.subscriber_id == bapOrgId) $ throwError AccessDenied
  now <- getCurrentTime
  (riderDetails, isNewRider) <- getRiderDetails req.customerMobileCountryCode req.customerPhoneNumber now
  rideRequest <-
    BP.buildRideReq
      (booking.id)
      (transporter.shortId)
      RideRequest.ALLOCATION
      now

  let finalTransaction addons = Esq.runTransaction $ do
        when isNewRider $ QRD.create riderDetails
        QRB.updateStatus booking.id SRB.CONFIRMED
        QRB.updateRiderId booking.id riderDetails.id
        QBL.updateAddress booking.fromLocationId req.fromAddress
        whenJust booking.discount $ \disc ->
          QDiscTransaction.create $ mkDiscountTransaction booking disc now
        addons

  fromLocation <- QBL.findById booking.fromLocationId >>= fromMaybeM LocationNotFound
  res <- case booking.bookingDetails of
    SRB.OneWayDetails details -> do
      finalTransaction $ do
        RideRequest.create rideRequest
        whenJust req.toAddress $ \toAddr -> QBL.updateAddress details.toLocationId toAddr
      toLocation <- QBL.findById details.toLocationId >>= fromMaybeM LocationNotFound
      return $
        DConfirmRes
          { toLocation = Just toLocation,
            bDetails = OneWayDetails,
            ..
          }
    SRB.RentalDetails details -> do
      let secondsPerMinute = 60
      schedulingReserveTime <- secondsToNominalDiffTime <$> asks (.schedulingReserveTime)
      let scheduledTime = addUTCTime (negate schedulingReserveTime) booking.startTime
          presentIntervalWidth = 5 * secondsPerMinute -- 5 minutes
      case compareTimeWithInterval presentIntervalWidth scheduledTime now of
        LT -> throwError $ InvalidRequest "impossible to book a ride for the past"
        EQ -> finalTransaction $ RideRequest.create rideRequest
        GT ->
          finalTransaction $
            createScheduleRentalRideRequestJob scheduledTime $
              AllocateRentalJobData
                { bookingId = booking.id,
                  shortOrgId = transporter.shortId
                }
      rentalFP <- QRFP.findById details.rentalFarePolicyId >>= fromMaybeM NoFarePolicy
      return $
        DConfirmRes
          { toLocation = Nothing,
            bDetails = RentalDetails {baseDistance = rentalFP.baseDistance, baseDuration = rentalFP.baseDuration},
            ..
          }
  let pickupPoint = booking.fromLocationId
      fareProductType = SRB.getFareProductType booking.bookingDetails
  driverPoolResults <- DrPool.recalculateDriverPool pickupPoint booking.id transporter.id booking.vehicleVariant fareProductType
  Esq.runTransaction $ traverse_ (QBE.logDriverInPoolEvent SB.ON_CONFIRM (Just booking.id)) driverPoolResults
  let driverPool = map (.driverId) driverPoolResults
  logTagInfo "OnConfirmCallback" $
    "Driver Pool for Ride " <> booking.id.getId <> " is set with drivers: "
      <> T.intercalate ", " (getId <$> driverPool)
  Esq.runTransaction $ QBE.logRideConfirmedEvent booking.id
  return res
  where
    createScheduleRentalRideRequestJob scheduledAt jobData =
      void $
        createJobByTime scheduledAt $
          JobEntry
            { jobType = AllocateRental,
              jobData = jobData,
              maxErrors = 5
            }

getRiderDetails :: (EncFlow m r, EsqDBFlow m r) => Text -> Text -> UTCTime -> m (SRD.RiderDetails, Bool)
getRiderDetails customerMobileCountryCode customerPhoneNumber now =
  QRD.findByMobileNumber customerPhoneNumber >>= \case
    Nothing -> fmap (,True) . encrypt =<< buildRiderDetails
    Just a -> return (a, False)
  where
    buildRiderDetails = do
      id <- generateGUID
      return $
        SRD.RiderDetails
          { id = id,
            mobileCountryCode = customerMobileCountryCode,
            mobileNumber = customerPhoneNumber,
            createdAt = now,
            updatedAt = now
          }

mkDiscountTransaction :: SRB.Booking -> Amount -> UTCTime -> DiscountTransaction
mkDiscountTransaction booking discount currTime =
  DiscountTransaction
    { bookingId = booking.id,
      organizationId = booking.providerId,
      discount = discount,
      createdAt = currTime
    }
