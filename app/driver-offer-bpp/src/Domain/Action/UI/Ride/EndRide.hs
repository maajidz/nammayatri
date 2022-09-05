module Domain.Action.UI.Ride.EndRide
  ( EndRideReq (..),
    ServiceHandle (..),
    endRideHandler,
  )
where

import Beckn.Prelude (roundToIntegral)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Data.OpenApi
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DrLoc
import Domain.Types.FareParams as Fare
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (pi)
import qualified SharedLogic.FareCalculator as Fare
import Types.App (Driver)
import Types.Error
import Utils.Common

newtype EndRideReq = EndRideReq
  { point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id Ride.Ride -> m (Maybe Ride.Ride),
    endRide :: Id SRB.Booking -> Ride.Ride -> Id Driver -> m (),
    notifyCompleteToBAP :: SRB.Booking -> Ride.Ride -> Fare.FareParameters -> Money -> m (),
    calculateFare ::
      Id Organization ->
      Variant ->
      Meters ->
      UTCTime ->
      Maybe Money ->
      m Fare.FareParameters,
    putDiffMetric :: Money -> Meters -> m (),
    findDriverLocById :: Id Person.Person -> m (Maybe DrLoc.DriverLocation),
    addLastWaypointAndRecalcDistanceOnEnd :: Id Person.Person -> LatLong -> m ()
  }

endRideHandler ::
  (MonadThrow m, Log m, MonadTime m) =>
  ServiceHandle m ->
  Id Person.Person ->
  Id Ride.Ride ->
  EndRideReq ->
  m APISuccess.APISuccess
endRideHandler ServiceHandle {..} requestorId rideId req = do
  requestor <- findById requestorId >>= fromMaybeM (PersonNotFound requestorId.getId)

  addLastWaypointAndRecalcDistanceOnEnd requestorId req.point
  -- here we update the current ride, so below we fetch the updated version

  ride <- findRideById (cast rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  case requestor.role of
    Person.DRIVER -> unless (requestorId == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (ride.status == Ride.INPROGRESS) $ throwError $ RideInvalidStatus "This ride cannot be ended"

  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  logTagInfo "endRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)

  now <- getCurrentTime
  (chargeableDistance, chargeableFare) <- recalculateFare booking ride

  let updRide =
        ride{tripEndTime = Just now,
             chargeableDistance = Just chargeableDistance,
             fare = Just chargeableFare
            }

  endRide booking.id updRide (cast driverId)

  notifyCompleteToBAP booking updRide booking.fareParams booking.estimatedFare

  return APISuccess.Success
  where
    recalculateFare booking ride = do
      let transporterId = booking.providerId
          actualDistance = roundToIntegral ride.traveledDistance
          oldDistance = booking.estimatedDistance

      -- maybe compare only distance fare?
      let estimatedBaseFare = booking.estimatedFare

      fareParams <- calculateFare transporterId booking.vehicleVariant actualDistance booking.estimatedFinishTime booking.fareParams.driverSelectedFare
      let updatedBaseFare = Fare.fareSumRounded fareParams
      let distanceDiff = actualDistance - oldDistance
      let fareDiff = updatedBaseFare - estimatedBaseFare
      logTagInfo "Fare recalculation" $
        "Fare difference: "
          <> show (realToFrac @_ @Double fareDiff)
          <> ", Distance difference: "
          <> show distanceDiff
      putDiffMetric fareDiff distanceDiff
      return (max actualDistance oldDistance, max updatedBaseFare estimatedBaseFare)
