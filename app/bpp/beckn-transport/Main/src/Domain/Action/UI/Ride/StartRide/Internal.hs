module Domain.Action.UI.Ride.StartRide.Internal where

import qualified Domain.Types.Booking.Type as SRB
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Ride as QRide

startRideTransaction :: EsqDBFlow m r => Id SP.Person -> Id SRide.Ride -> Id SRB.Booking -> LatLong -> m ()
startRideTransaction driverId rId bookingId firstPoint = Esq.runTransaction $ do
  QRide.updateStatus rId SRide.INPROGRESS
  QRide.updateStartTimeAndLoc rId firstPoint
  QBE.logRideCommencedEvent (cast driverId) bookingId rId
  now <- getCurrentTime
  void $ DrLoc.upsertGpsCoord driverId firstPoint now
