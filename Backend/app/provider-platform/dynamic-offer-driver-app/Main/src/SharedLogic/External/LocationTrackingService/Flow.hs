{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Flow where

-- import qualified Data.Text as T

import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverLocation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.API.DriverDetails as DriverDetailsAPI
import qualified SharedLogic.External.LocationTrackingService.API.DriversLocation as DriversLocationAPI
import qualified SharedLogic.External.LocationTrackingService.API.EndRide as EndRideAPI
import qualified SharedLogic.External.LocationTrackingService.API.NearBy as NearByAPI
import qualified SharedLogic.External.LocationTrackingService.API.RideDetails as RideDetailsAPI
import qualified SharedLogic.External.LocationTrackingService.API.StartRide as StartRideAPI
import SharedLogic.External.LocationTrackingService.Types

rideStart :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> Id DR.Ride -> Double -> Double -> Id DM.Merchant -> Id DP.Person -> m APISuccess
rideStart rsCfg rideId lat lon merchantId driverId = do
  let url = rsCfg.url
  let req =
        StartRideReq
          { lat,
            lon,
            merchantId,
            driverId
          }
  callAPI url (StartRideAPI.startRide rideId req) "rideStart" StartRideAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_START_RIDE_API") url)

rideEnd :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> Id DR.Ride -> Double -> Double -> Id DM.Merchant -> Id DP.Person -> m EndRideRes
rideEnd rsCfg rideId lat lon merchantId driverId = do
  let url = rsCfg.url
  let req =
        EndRideReq
          { lat,
            lon,
            merchantId,
            driverId
          }
  callAPI url (EndRideAPI.endRide rideId req) "rideEnd" EndRideAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_END_RIDE_API") url)

nearBy :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> Double -> Double -> Bool -> Maybe Variant -> Int -> Id DM.Merchant -> m [DriverLocation]
nearBy rsCfg lat lon onRide vt radius merchantId = do
  let url = rsCfg.url
  let req =
        NearByReq
          { lat,
            lon,
            onRide,
            radius,
            vehicleType = vt,
            merchantId = merchantId
          }
  callAPI url (NearByAPI.nearBy req) "nearBy" NearByAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NEAR_BY_API") url)

driverDetails :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> Id DP.Person -> DI.DriverMode -> m APISuccess
driverDetails rsCfg driverId driverMode = do
  let url = rsCfg.url
  let req =
        DriverDetailsReq
          { driverId,
            driverMode
          }
  callAPI url (DriverDetailsAPI.driverDetails req) "driverDetails" DriverDetailsAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_DRIVER_DETAILS_API") url)

rideDetails :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> Id DR.Ride -> DR.RideStatus -> Id DM.Merchant -> Id DP.Person -> Double -> Double -> m APISuccess
rideDetails rsCfg rideId rideStatus merchantId driverId lat lon = do
  let url = rsCfg.url
  let req =
        RideDetailsReq
          { rideId,
            rideStatus,
            merchantId,
            driverId,
            lat,
            lon
          }
  callAPI url (RideDetailsAPI.rideDetails req) "rideDetails" RideDetailsAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_RIDE_DETAILS_API") url)

driversLocation :: (CoreMetrics m, MonadFlow m) => LocationTrackingeServiceConfig -> [Id DP.Person] -> m [DriverLocation]
driversLocation rsCfg driverIds = do
  let url = rsCfg.url
  let req =
        DriversLocationReq
          { driverIds
          }
  callAPI url (DriversLocationAPI.driversLocation req) "driversLocation" DriversLocationAPI.locationTrackingServiceAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_DRIVERS_LOCATION_API") url)

findByDriverId :: [DriverLocation] -> Id DP.Person -> Maybe DriverLocation
findByDriverId driverLocations driverId = find (\driverLocation -> driverId == driverLocation.driverId) driverLocations
