{-# LANGUAGE DerivingVia #-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
    calculateDriverPoolWithActualDist,
    incrementTotalQuotesCount,
    incrementQuoteAcceptedCount,
    getTotalQuotesSent,
    getLatestAcceptanceRatio,
    incrementTotalRidesCount,
    isThresholdRidesCompleted,
    incrementCancellationCount,
    getLatestCancellationRatio,
    getCurrentWindowAvailability,
    getQuotesCount,
    getPopupDelay,
    addSearchRequestValidTillToCache,
    getValidSearchRequestCount,
    removeSearchReqIdFromMap,
    PoolCalculationStage (..),
    module Reexport,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
import qualified Beckn.Utils.SlidingWindowCounters as SWC
import Data.List (partition)
import Data.List.Extra (notNull)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.SearchRequest
import qualified Domain.Types.TransporterConfig as DTC
import Domain.Types.Vehicle.Variant (Variant)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.TransporterConfig as TC
import qualified Storage.Queries.Person as QP
import Tools.Maps as Maps
import Tools.Metrics

data PoolCalculationStage = Estimate | DriverSelection

mkTotalQuotesKey :: Text -> Text
mkTotalQuotesKey driverId = "driver-offer:DriverPool:Total-quotes:DriverId-" <> driverId

mkQuotesAcceptedKey :: Text -> Text
mkQuotesAcceptedKey driverId = "driver-offer:DriverPool:Quote-accepted:DriverId-" <> driverId

mkTotalRidesKey :: Text -> Text
mkTotalRidesKey driverId = "driver-offer:DriverPool:Total-Rides:DriverId-" <> driverId

mkRideCancelledKey :: Text -> Text
mkRideCancelledKey driverId = "driver-offer:DriverPool:Ride-cancelled:DriverId-" <> driverId

mkAvailableTimeKey :: Text -> Text
mkAvailableTimeKey driverId = "driver-offer:DriverPool:Available-time:DriverId-" <> driverId

windowFromTransporterConfig :: (L.MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> (DTC.TransporterConfig -> SWC.SlidingWindowOptions) -> m SWC.SlidingWindowOptions
windowFromTransporterConfig merchantId windowKey = maybe defaultWindow windowKey <$> TC.findByMerchantId merchantId
  where
    defaultWindow = SWC.SlidingWindowOptions 7 SWC.Days

withAcceptanceRatioWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withAcceptanceRatioWindowOption merchantId fn = windowFromTransporterConfig merchantId (.acceptanceRatioWindowOption) >>= fn

withCancellationRatioWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withCancellationRatioWindowOption merchantId fn = windowFromTransporterConfig merchantId (.cancellationRatioWindowOption) >>= fn

withAvailabilityTimeWindowOption ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withAvailabilityTimeWindowOption merchantId fn = windowFromTransporterConfig merchantId (.availabilityTimeWindowOption) >>= fn

withMinQuotesToQualifyIntelligentPoolWindowOption ::
  ( Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r
  ) =>
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withMinQuotesToQualifyIntelligentPoolWindowOption fn = do
  asks (.intelligentPoolConfig.minQuotesToQualifyForIntelligentPoolWindowOption) >>= fn

incrementTotalQuotesCount ::
  ( Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m ()
incrementTotalQuotesCount merchantId driverId =
  Redis.withCrossAppRedis do
    withAcceptanceRatioWindowOption merchantId $ SWC.incrementWindowCount (mkTotalQuotesKey driverId.getId) -- for acceptance ratio calculation
    withMinQuotesToQualifyIntelligentPoolWindowOption $ SWC.incrementWindowCount (mkQuotesCountKey driverId.getId) -- total quotes sent count in different sliding window (used in driver pool for random vs intelligent filtering)

mkParallelSearchRequestKey :: Id DM.Merchant -> Id DP.Driver -> Text
mkParallelSearchRequestKey mId dId = "driver-offer:DriverPool:Search-Req-Validity-Map-" <> mId.getId <> dId.getId

addSearchRequestValidTillToCache ::
  ( Redis.HedisFlow m r,
    HasSendSearchRequestJobConfig r
  ) =>
  Id SearchRequest ->
  Id DM.Merchant ->
  Id DP.Driver ->
  UTCTime ->
  m ()
addSearchRequestValidTillToCache searchReqId merchantId driverId validTill = do
  singleBatchProcessTime <- fromIntegral <$> asks (.sendSearchRequestJobCfg.singleBatchProcessTime)
  Redis.withCrossAppRedis $ Redis.hSetExp (mkParallelSearchRequestKey merchantId driverId) searchReqId.getId validTill singleBatchProcessTime

getValidSearchRequestCount ::
  ( Redis.HedisFlow m r,
    MonadReader r m
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  UTCTime ->
  m Int
getValidSearchRequestCount merchantId driverId now = Redis.withCrossAppRedis $ do
  let key = mkParallelSearchRequestKey merchantId driverId
  searchRequestValidityMap :: [(Text, UTCTime)] <- Redis.hGetAll key
  let (valid, old) = partition ((> now) . snd) searchRequestValidityMap
  when (notNull old) $ Redis.hDel key (map fst old)
  pure $ length valid

removeSearchReqIdFromMap ::
  ( Redis.HedisFlow m r,
    MonadTime m
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  Id SearchRequest ->
  m ()
removeSearchReqIdFromMap merchantId driverId = Redis.withCrossAppRedis . Redis.hDel (mkParallelSearchRequestKey merchantId $ cast driverId) . (: []) .(.getId)

incrementQuoteAcceptedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m ()
incrementQuoteAcceptedCount merchantId driverId = Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantId $ SWC.incrementWindowCount (mkQuotesAcceptedKey driverId.getId)

getTotalQuotesSent ::
  ( Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m Int
getTotalQuotesSent merchantId driverId =
  sum . catMaybes <$> (Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantId $ SWC.getCurrentWindowValues (mkTotalQuotesKey driverId.getId))

getLatestAcceptanceRatio ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m Double
getLatestAcceptanceRatio merchantId driverId = Redis.withCrossAppRedis . withAcceptanceRatioWindowOption merchantId $ SWC.getLatestRatio (getId driverId) mkQuotesAcceptedKey mkTotalQuotesKey

incrementTotalRidesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m ()
incrementTotalRidesCount merchantId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantId $ SWC.incrementWindowCount (mkTotalRidesKey driverId.getId)

getTotalRidesCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m Int
getTotalRidesCount merchantId driverId = sum . catMaybes <$> (Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantId $ SWC.getCurrentWindowValues (mkTotalRidesKey driverId.getId))

incrementCancellationCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Person ->
  m ()
incrementCancellationCount merchantId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantId $ SWC.incrementWindowCount (mkRideCancelledKey driverId.getId)

getLatestCancellationRatio ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m Double
getLatestCancellationRatio merchantId driverId = Redis.withCrossAppRedis . withCancellationRatioWindowOption merchantId $ SWC.getLatestRatio driverId.getId mkRideCancelledKey mkTotalRidesKey

getCurrentWindowAvailability ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    FromJSON a
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  m [Maybe a]
getCurrentWindowAvailability merchantId driverId = Redis.withCrossAppRedis . withAvailabilityTimeWindowOption merchantId $ SWC.getCurrentWindowValues (mkAvailableTimeKey driverId.getId)

mkQuotesCountKey :: Text -> Text
mkQuotesCountKey driverId = "driver-offer:DriverPool:Total-quotes-sent:DriverId-" <> driverId

getQuotesCount ::
  ( L.MonadFlow m,
    Redis.HedisFlow m r,
    Reexport.HasDriverPoolConfig r,
    FromJSON a,
    Num a
  ) =>
  Id DP.Driver ->
  m a
getQuotesCount driverId = sum . catMaybes <$> (Redis.withCrossAppRedis . withMinQuotesToQualifyIntelligentPoolWindowOption $ SWC.getCurrentWindowValues (mkQuotesCountKey driverId.getId))

isThresholdRidesCompleted ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Reexport.HasDriverPoolConfig r
  ) =>
  Id DP.Driver ->
  Id DM.Merchant ->
  RideRequestPopupConfig ->
  m Bool
isThresholdRidesCompleted driverId merchantId rideRequestPopupConfig = do
  let thresholdRidesCount = fromMaybe 5 rideRequestPopupConfig.thresholdRidesCount
  totalRides <- getTotalRidesCount merchantId driverId
  pure $ totalRides >= thresholdRidesCount

getPopupDelay ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Reexport.HasDriverPoolConfig r
  ) =>
  Id DM.Merchant ->
  Id DP.Driver ->
  Double ->
  RideRequestPopupConfig ->
  m Seconds
getPopupDelay merchantId driverId cancellationRatio rideRequestPopupConfig = do
  let cancellationRatioThreshold = fromIntegral $ fromMaybe 40 rideRequestPopupConfig.thresholdCancellationScore
  (rideRequestPopupConfig.defaultPopupDelay +)
    <$> if cancellationRatio * 100 > cancellationRatioThreshold
      then do
        isThresholdRidesDone <- isThresholdRidesCompleted driverId merchantId rideRequestPopupConfig
        pure $
          if isThresholdRidesDone
            then fromMaybe (Seconds 0) rideRequestPopupConfig.popupDelayToAddAsPenalty
            else Seconds 0
      else pure $ Seconds 0

calculateDriverPool ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasCoordinates a,
    HasMaxParallelSearchRequests r
  ) =>
  PoolCalculationStage ->
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  Maybe PoolRadiusStep ->
  m [DriverPoolResult]
calculateDriverPool poolStage driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep = do
  let radius = getRadius mRadiusStep
  let coord = getCoordinates pickup
  now <- getCurrentTime
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      Esq.runInReplica $
        QP.getNearestDrivers
          mbVariant
          coord
          radius
          merchantId
          onlyNotOnRide
          driverPoolCfg.driverPositionInfoExpiry
  maxParallelSearchRequests <- asks (.maxParallelSearchRequests)
  driversWithLessThanNParallelRequests <- case poolStage of
    DriverSelection -> filter ((< maxParallelSearchRequests) . fst) <$> mapM (getParallelSearchRequestCount now) approxDriverPool
    Estimate -> pure $ map (0,) approxDriverPool --estimate stage we dont need to consider actual parallel request counts
  pure $ makeDriverPoolResult <$> driversWithLessThanNParallelRequests
  where
    getParallelSearchRequestCount now dObj = do
      parallelSearchRequestCount <- getValidSearchRequestCount merchantId (cast dObj.driverId) now
      pure (parallelSearchRequestCount, dObj)
    getRadius mRadiusStep_ = do
      let maxRadius = fromIntegral driverPoolCfg.maxRadiusOfSearch
      case mRadiusStep_ of
        Just radiusStep -> do
          let minRadius = fromIntegral driverPoolCfg.minRadiusOfSearch
          let radiusStepSize = fromIntegral driverPoolCfg.radiusStepSize
          min (minRadius + radiusStepSize * radiusStep) maxRadius
        Nothing -> maxRadius
    makeDriverPoolResult :: (Int, QP.NearestDriversResult) -> DriverPoolResult
    makeDriverPoolResult (parallelSearchRequestCount, QP.NearestDriversResult {..}) = do
      DriverPoolResult
        { distanceToPickup = distanceToDriver,
          variant = variant,
          ..
        }

calculateDriverPoolWithActualDist ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasDriverPoolConfig r,
    HasCoordinates a
  ) =>
  PoolCalculationStage ->
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  Maybe PoolRadiusStep ->
  m [DriverPoolWithActualDistResult]
calculateDriverPoolWithActualDist poolCalculationStage driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep = do
  driverPool <- calculateDriverPool poolCalculationStage driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep
  case driverPool of
    [] -> return []
    (a : pprox) -> do
      driverPoolWithActualDist <- computeActualDistance merchantId pickup (a :| pprox)
      let filtDriverPoolWithActualDist = case driverPoolCfg.actualDistanceThreshold of
            Nothing -> NE.toList driverPoolWithActualDist
            Just threshold -> NE.filter (filterFunc threshold) driverPoolWithActualDist
      logDebug $ "secondly filtered driver pool" <> show filtDriverPoolWithActualDist
      return filtDriverPoolWithActualDist
  where
    filterFunc threshold estDist = getMeters estDist.actualDistanceToPickup <= fromIntegral threshold

computeActualDistance ::
  ( CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasDriverPoolConfig r,
    HasCoordinates a
  ) =>
  Id DM.Merchant ->
  a ->
  NonEmpty DriverPoolResult ->
  m (NonEmpty DriverPoolWithActualDistResult)
computeActualDistance orgId pickup driverPoolResults = do
  let pickupLatLong = getCoordinates pickup
  rideRequestPopupConfig <- asks (.rideRequestPopupConfig)
  getDistanceResults <-
    Maps.getEstimatedPickupDistances orgId $
      Maps.GetDistancesReq
        { origins = driverPoolResults,
          destinations = pickupLatLong :| [],
          travelMode = Just Maps.CAR
        }
  logDebug $ "get distance results" <> show getDistanceResults
  return $ mkDriverPoolWithActualDistResult rideRequestPopupConfig <$> getDistanceResults
  where
    mkDriverPoolWithActualDistResult rideRequestPopupConfig distDur = do
      DriverPoolWithActualDistResult
        { driverPoolResult = distDur.origin,
          actualDistanceToPickup = distDur.distance,
          actualDurationToPickup = distDur.duration,
          rideRequestPopupDelayDuration = rideRequestPopupConfig.defaultPopupDelay,
          cancellationRatio = Nothing,
          isPartOfIntelligentPool = False
        }
