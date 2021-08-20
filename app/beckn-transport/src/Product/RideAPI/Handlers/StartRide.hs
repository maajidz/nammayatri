module Product.RideAPI.Handlers.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import EulerHS.Prelude
import Types.Error
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import Utils.Common
import qualified Types.Storage.Quote as SQuote

data ServiceHandle m = ServiceHandle
  { findPersonById :: Id Person.Person -> m (Maybe Person.Person),
    findRideBookingById :: Id SRB.RideBooking -> m (Maybe SRB.RideBooking),
    findRideById :: Id SRide.Ride -> m (Maybe SRide.Ride),
    findQuoteById :: Id SQuote.Quote -> m (Maybe SQuote.Quote),
    startRide :: Id SRide.Ride -> m (),
    notifyBAPRideStarted :: SQuote.Quote -> SRB.RideBooking -> SRide.Ride -> m (),
    rateLimitStartRide :: Id Person.Person -> Id SRide.Ride -> m ()
  }

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Id Person.Person -> Id SRide.Ride -> Text -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} requestorId rideId otp = do
  rateLimitStartRide requestorId rideId
  requestor <- findPersonById requestorId >>= fromMaybeM PersonNotFound
  ride <- findRideById rideId >>= fromMaybeM RideDoesNotExist
  case requestor.role of
    Person.DRIVER -> do
      let rideDriver = ride.driverId
      unless (rideDriver == requestorId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "This ride cannot be started"
  rideBooking <- findRideBookingById ride.bookingId >>= fromMaybeM QuoteNotFound
  let inAppOtp = ride.otp
  when (otp /= inAppOtp) $ throwError IncorrectOTP
  logTagInfo "startRide" ("DriverId " <> getId requestorId <> ", RideId " <> getId rideId)
  startRide ride.id
  quote <- findQuoteById rideBooking.quoteId >>= fromMaybeM QuoteNotFound
  notifyBAPRideStarted quote rideBooking ride{status = SRide.INPROGRESS}
  pure APISuccess.Success
  where
    isValidRideStatus status = status == SRide.NEW
