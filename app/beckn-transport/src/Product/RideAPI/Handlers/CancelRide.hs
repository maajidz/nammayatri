module Product.RideAPI.Handlers.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance (ProductInstance, ProductInstanceStatus (..))
import EulerHS.Prelude
import Types.App (Ride)
import Types.Error
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

data ServiceHandle m = ServiceHandle
  { findPIById :: Id ProductInstance -> m ProductInstance,
    findPersonById :: Id Person.Person -> m Person.Person,
    cancelRide :: Id Ride -> Bool -> m ()
  }

cancelRideHandler :: MonadHandler m => ServiceHandle m -> Text -> Id Ride -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} authorizedEntityId rideId = do
  prodInst <- findPIById $ cast rideId
  unless (isValidPI prodInst) $ throwError $ PIInvalidStatus "This ride cannot be canceled"
  authPerson <- findPersonById $ Id authorizedEntityId
  case authPerson.role of
    Person.ADMIN -> cancelRide rideId False
    Person.DRIVER -> do
      driverId <- prodInst.personId & fromMaybeM (PIFieldNotPresent "person")
      unless (authPerson.id == driverId) $ throwError NotAnExecutor
      cancelRide rideId True
    _ -> throwError AccessDenied
  pure APISuccess.Success
  where
    isValidPI prodInst =
      prodInst._type == Case.RIDEORDER
        && (prodInst.status) `elem` [CONFIRMED, TRIP_ASSIGNED, TRIP_REASSIGNMENT]
