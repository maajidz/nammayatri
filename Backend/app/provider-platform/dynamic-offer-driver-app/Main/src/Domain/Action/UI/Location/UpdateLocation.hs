{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Location.UpdateLocation
  ( UpdateLocationReq,
    Waypoint (..),
    UpdateLocationHandle (..),
    buildUpdateLocationHandle,
    updateLocationHandler,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.DriverInformation as DDInfo
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import GHC.Records.Extra
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.SlidingWindowLimiter (slidingWindowLimiter)
import qualified Lib.LocationUpdates as LocUpd
import qualified SharedLogic.DriverLocation as DrLoc
import SharedLogic.DriverPool (updateDriverSpeedInRedis)
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.DriverInformation as DInfo
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTConf
import qualified Storage.Queries.Person as QP
import Tools.Metrics (CoreMetrics)

type UpdateLocationReq = NonEmpty Waypoint

-- Short field names for lesser json array size:
data Waypoint = Waypoint
  { pt :: LatLong, -- point
    ts :: UTCTime, -- timestamp
    acc :: Maybe Double -- accuracy, optional for now
  }
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema, PrettyShow)

data UpdateLocationHandle m = UpdateLocationHandle
  { driver :: Person.Person,
    findDriverLocation :: m (Maybe DriverLocation),
    upsertDriverLocation :: LatLong -> UTCTime -> Id DM.Merchant -> m (),
    getAssignedRide :: m (Maybe (Id DRide.Ride, DRide.RideStatus)),
    addIntermediateRoutePoints :: Id DRide.Ride -> NonEmpty LatLong -> m ()
  }

data DriverLocationUpdateStreamData = DriverLocationUpdateStreamData
  { rId :: Maybe Text, -- rideId
    mId :: Text, -- merchantId
    ts :: UTCTime, -- timestamp
    st :: UTCTime, -- systemtime when location update recieved
    pt :: LatLong, -- lat log
    da :: Bool, -- driver avaiable
    mode :: Maybe DDInfo.DriverMode
  }
  deriving (Generic, FromJSON, ToJSON)

buildUpdateLocationHandle ::
  Id Person.Person ->
  Flow (UpdateLocationHandle Flow)
buildUpdateLocationHandle driverId = do
  driver <-
    runInReplica $
      QP.findById driverId
        >>= fromMaybeM (PersonNotFound driverId.getId)
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler driver.merchantId False
  pure $
    UpdateLocationHandle
      { driver,
        findDriverLocation = DrLoc.findById driver.merchantId driverId,
        upsertDriverLocation = DrLoc.upsertGpsCoord driverId,
        getAssignedRide = SRide.getInProgressOrNewRideIdAndStatusByDriverId driverId,
        addIntermediateRoutePoints = \rideId ->
          LocUpd.addIntermediateRoutePoints defaultRideInterpolationHandler rideId driverId
      }

streamLocationUpdates ::
  ( MonadFlow m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["driverLocationUpdateTopic" ::: Text]
  ) =>
  Maybe (Id DRide.Ride) ->
  Id DM.Merchant ->
  Id Person.Person ->
  LatLong ->
  UTCTime ->
  Bool ->
  Maybe DDInfo.DriverMode ->
  m ()
streamLocationUpdates mbRideId merchantId driverId point timestamp isDriverActive mbDriverMode = do
  topicName <- asks (.driverLocationUpdateTopic)
  now <- getCurrentTime
  produceMessage
    (topicName, Just (encodeUtf8 $ getId driverId))
    (DriverLocationUpdateStreamData (getId <$> mbRideId) (getId merchantId) timestamp now point isDriverActive mbDriverMode)

updateLocationHandler ::
  ( Redis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasFlowEnv m r '["driverLocationUpdateRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["driverLocationUpdateTopic" ::: Text],
    MonadTime m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  UpdateLocationHandle m ->
  UpdateLocationReq ->
  m APISuccess
updateLocationHandler UpdateLocationHandle {..} waypoints = withLogTag "driverLocationUpdate" $
  withLogTag ("driverId-" <> driver.id.getId) $ do
    driverInfo <- DInfo.findById (cast driver.id) >>= fromMaybeM (PersonNotFound driver.id.getId)
    logInfo $ "got location updates: " <> getId driver.id <> " " <> encodeToText waypoints
    checkLocationUpdatesRateLimit driver.id
    thresholdConfig <- QTConf.findByMerchantId driver.merchantId >>= fromMaybeM (TransporterConfigNotFound driver.merchantId.getId)
    let minLocationAccuracy = thresholdConfig.minLocationAccuracy
    unless (driver.role == Person.DRIVER) $ throwError AccessDenied
    LocUpd.whenWithLocationUpdatesLock driver.id $ do
      mbOldLoc <- findDriverLocation
      let sortedWaypoint = toList $ NE.sortWith (.ts) waypoints
          filteredWaypoint = maybe sortedWaypoint (\oldLoc -> filter ((oldLoc.coordinatesCalculatedAt <) . (.ts)) sortedWaypoint) mbOldLoc
      mbRideIdAndStatus <- getAssignedRide

      case filteredWaypoint of
        [] -> logWarning "Incoming points are older than current one, ignoring"
        (a : ax) -> do
          let newWaypoints = a :| ax
              currPoint = NE.last newWaypoints
          upsertDriverLocation currPoint.pt currPoint.ts driver.merchantId
          fork "updating in kafka" $
            forM_ (a : ax) $ \point -> do
              streamLocationUpdates (fst <$> mbRideIdAndStatus) driver.merchantId driver.id point.pt point.ts driverInfo.active driverInfo.mode

      let filteredWaypointWithAccuracy = filter (\val -> fromMaybe 0.0 val.acc <= minLocationAccuracy) filteredWaypoint
      case filteredWaypointWithAccuracy of
        [] -> logWarning "Accuracy of the points is low, ignoring"
        (a : ax) -> do
          let newWaypoints = a :| ax
          fork "update driver speed in redis" $
            forM_ (a : ax) $ \point -> do
              updateDriverSpeedInRedis driver.merchantId driver.id point.pt point.ts
          maybe
            (logInfo "No ride is assigned to driver, ignoring")
            (\(rideId, rideStatus) -> when (rideStatus == DRide.INPROGRESS) $ addIntermediateRoutePoints rideId $ NE.map (.pt) newWaypoints)
            mbRideIdAndStatus

    pure Success

checkLocationUpdatesRateLimit ::
  ( Redis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasFlowEnv m r '["driverLocationUpdateRateLimitOptions" ::: APIRateLimitOptions],
    MonadTime m
  ) =>
  Id Person.Person ->
  m ()
checkLocationUpdatesRateLimit personId = do
  let key = locationUpdatesHitsCountKey personId
  hitsLimit <- asks (.driverLocationUpdateRateLimitOptions.limit)
  limitResetTimeInSec <- asks (.driverLocationUpdateRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    logError "Location updates hitting limit, ignoring"
    throwError $ HitsLimitError limitResetTimeInSec

locationUpdatesHitsCountKey :: Id Person.Person -> Text
locationUpdatesHitsCountKey personId = "BPP:DriverLocationUpdates:" <> getId personId <> ":hitsCount"
