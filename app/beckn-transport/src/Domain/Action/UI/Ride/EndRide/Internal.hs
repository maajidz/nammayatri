module Domain.Action.UI.Ride.EndRide.Internal where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Booking.Type as SRB
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.Ride as SRide
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.FarePolicy.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import Types.App (Driver)

endRideTransaction :: EsqDBFlow m r => Id SRB.Booking -> SRide.Ride -> Id Driver -> [DFareBreakup.FareBreakup] -> m ()
endRideTransaction bookingId ride driverId fareBreakups = Esq.runTransaction $ do
  QRide.updateAll ride.id ride
  QRide.updateStatus ride.id SRide.COMPLETED
  QRB.updateStatus bookingId SRB.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
  traverse_ QFareBreakup.create fareBreakups
