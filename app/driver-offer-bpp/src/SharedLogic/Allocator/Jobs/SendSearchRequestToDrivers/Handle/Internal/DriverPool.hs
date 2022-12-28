module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool
  ( isBatchNumExceedLimit,
    cleanupDriverPoolBatches,
    getNextDriverPoolBatch,
    getPoolBatchNum,
    module Reexport,
  )
where

import Beckn.Randomizer (randomizeList)
import Beckn.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude hiding (id)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as Reexport
import SharedLogic.DriverPool
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import Tools.Maps as Maps
import Tools.Metrics

isBatchNumExceedLimit ::
  ( Redis.HedisFlow m r,
    HasSendSearchRequestJobConfig r
  ) =>
  Id DSR.SearchRequest ->
  m Bool
isBatchNumExceedLimit searchReqId = do
  maxNumberOfBatches <- asks (.sendSearchRequestJobCfg.driverPoolBatchesCfg.maxNumberOfBatches)
  currentBatchNum <- getPoolBatchNum searchReqId
  return $ currentBatchNum >= maxNumberOfBatches

driverPoolKey :: Id DSR.SearchRequest -> Text
driverPoolKey searchReqId = "Driver-Offer:DriverPool:SearchReqId-" <> searchReqId.getId

driverPoolBatchKey :: Id DSR.SearchRequest -> PoolBatchNum -> Text
driverPoolBatchKey searchReqId batchNum = driverPoolKey searchReqId <> ":BatchNum-" <> show batchNum

prepareDriverPoolBatch ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r,
    HasSendSearchRequestJobConfig r,
    HasField "windowOptions" r SWC.SlidingWindowOptions
  ) =>
  DSR.SearchRequest ->
  PoolBatchNum ->
  m [DriverPoolWithActualDistResult]
prepareDriverPoolBatch searchReq batchNum = withLogTag ("BatchNum-" <> show batchNum) $ do
  previousBatchesDrivers <- getPreviousBatchesDrivers
  logDebug $ "PreviousBatchesDrivers-" <> show previousBatchesDrivers
  prepareDriverPoolBatch' previousBatchesDrivers
  where
    prepareDriverPoolBatch' previousBatchesDrivers = do
      radiusStep <- getPoolRadiusStep searchReq.id
      driverPool <- calcDriverPool radiusStep
      logDebug $ "DriverPool-" <> show driverPool
      sortedDriverPool <- sortFunction driverPool
      logDebug $ "SortedDriverPool-" <> show sortedDriverPool
      let onlyNewDriversPool = filter (\dpr -> dpr.driverPoolResult.driverId `notElem` previousBatchesDrivers) sortedDriverPool
      logDebug $ "OnlyNewDriversPool-" <> show onlyNewDriversPool
      driverPoolBatch <- getBatch onlyNewDriversPool
      logDebug $ "DriverPoolBatch-" <> show driverPoolBatch
      batchSize <- asks (.sendSearchRequestJobCfg.driverPoolBatchesCfg.driverBatchSize)
      if length driverPoolBatch < batchSize
        then do
          isAtMaxRadiusStep' <- isAtMaxRadiusStep radiusStep
          if isAtMaxRadiusStep'
            then do
              filledBatch <- fillBatch batchSize sortedDriverPool driverPoolBatch
              logDebug $ "FilledDriverPoolBatch-" <> show filledBatch
              cacheBatch filledBatch
              return filledBatch
            else do
              incrementPoolRadiusStep searchReq.id
              prepareDriverPoolBatch' previousBatchesDrivers
        else do
          cacheBatch driverPoolBatch
          return driverPoolBatch

    calcDriverPool radiusStep = do
      let vehicleVariant = searchReq.vehicleVariant
          merchantId = searchReq.providerId
      let pickupLoc = searchReq.fromLocation
      let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
      calculateDriverPoolWithActualDist (Just vehicleVariant) pickupLatLong merchantId True (Just radiusStep)
    getBatch driverPool = do
      batchSize <- asks (.sendSearchRequestJobCfg.driverPoolBatchesCfg.driverBatchSize)
      return $ take batchSize driverPool
    fillBatch batchSize driverPool batch = do
      let batchDriverIds = batch <&> (.driverPoolResult.driverId)
      let driversToFillBatch = take (batchSize - length batch) $ filter (\dpr -> dpr.driverPoolResult.driverId `notElem` batchDriverIds) driverPool
      return $ batch <> driversToFillBatch
    cacheBatch batch = do
      logDebug $ "Caching batch-" <> show batch
      Redis.withCrossAppRedis $ Redis.setExp (driverPoolBatchKey searchReq.id batchNum) batch (60 * 10)
    sortFunction driverPool = do
      sortingType <- asks (.sendSearchRequestJobCfg.driverPoolBatchesCfg.poolSortingType)
      case sortingType of
        ByAcceptanceRatio -> intelligentPoolSelection driverPool
        ByRandom -> randomizeAndLimitSelection driverPool
    isAtMaxRadiusStep radiusStep = do
      minRadiusOfSearch <- fromIntegral @_ @Double <$> asks (.driverPoolCfg.minRadiusOfSearch)
      maxRadiusOfSearch <- fromIntegral @_ @Double <$> asks (.driverPoolCfg.maxRadiusOfSearch)
      radiusStepSize <- fromIntegral @_ @Double <$> asks (.driverPoolCfg.radiusStepSize)
      let maxRadiusStep = ceiling $ (maxRadiusOfSearch - minRadiusOfSearch) / radiusStepSize
      return $ maxRadiusStep <= radiusStep
    getPreviousBatchesDrivers = do
      batches <- forM [0 .. (batchNum - 1)] \num -> do
        getDriverPoolBatch searchReq.id num
      return $ (.driverPoolResult.driverId) <$> concat batches

getDriverPoolBatch ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  PoolBatchNum ->
  m [DriverPoolWithActualDistResult]
getDriverPoolBatch searchReqId batchNum = do
  Redis.withCrossAppRedis $
    Redis.get (driverPoolBatchKey searchReqId batchNum)
      >>= maybe whenFoundNothing whenFoundSomething
  where
    whenFoundNothing = do
      logWarning "Unexpected empty driver pool batch cache."
      return []
    whenFoundSomething = \case
      [] -> do
        logWarning "Unexpected empty driver pool batch."
        return []
      a -> return a

intelligentPoolSelection ::
  ( Redis.HedisFlow m r,
    HasField "windowOptions" r SWC.SlidingWindowOptions,
    MonadFlow m
  ) =>
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
intelligentPoolSelection dp =
  map snd
    . sortOn (Down . fst)
    <$> ( (\poolWithRatio -> logInfo ("Drivers in Pool with acceptance ratios " <> show poolWithRatio) $> poolWithRatio)
            =<< mapM (\dPoolRes -> (,dPoolRes) <$> getLatestAcceptanceRatio dPoolRes.driverPoolResult.driverId) dp
        )

randomizeAndLimitSelection ::
  (MonadFlow m) =>
  [DriverPoolWithActualDistResult] ->
  m [DriverPoolWithActualDistResult]
randomizeAndLimitSelection = randomizeList

poolBatchNumKey :: Id DSR.SearchRequest -> Text
poolBatchNumKey searchReqId = "Driver-Offer:Allocator:PoolBatchNum:SearchReqId-" <> searchReqId.getId

poolRadiusStepKey :: Id DSR.SearchRequest -> Text
poolRadiusStepKey searchReqId = "Driver-Offer:Allocator:PoolRadiusStep:SearchReqId-" <> searchReqId.getId

cleanupDriverPoolBatches ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  m ()
cleanupDriverPoolBatches searchReqId = do
  Redis.withCrossAppRedis $ do
    Redis.delByPattern (driverPoolKey searchReqId <> "*")
    Redis.del (poolRadiusStepKey searchReqId)
    Redis.del (poolBatchNumKey searchReqId)
  logInfo "Cleanup redis."

getNextDriverPoolBatch ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r,
    HasSendSearchRequestJobConfig r,
    HasField "windowOptions" r SWC.SlidingWindowOptions
  ) =>
  DSR.SearchRequest ->
  m [DriverPoolWithActualDistResult]
getNextDriverPoolBatch searchReq = withLogTag "getNextDriverPoolBatch" do
  batchNum <- getPoolBatchNum searchReq.id
  incrementBatchNum searchReq.id
  prepareDriverPoolBatch searchReq batchNum

getPoolBatchNum :: (Redis.HedisFlow m r) => Id DSR.SearchRequest -> m PoolBatchNum
getPoolBatchNum searchReqId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolBatchNumKey searchReqId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolBatchNumKey searchReqId) (0 :: Integer) expTime
      return 0

incrementBatchNum ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  m ()
incrementBatchNum searchReqId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolBatchNumKey searchReqId)
  logInfo $ "Increment batch num to " <> show res <> "."
  return ()

getPoolRadiusStep :: (Redis.HedisFlow m r) => Id DSR.SearchRequest -> m PoolRadiusStep
getPoolRadiusStep searchReqId = do
  res <- Redis.withCrossAppRedis $ Redis.get (poolRadiusStepKey searchReqId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.withCrossAppRedis $ Redis.setExp (poolRadiusStepKey searchReqId) (0 :: Integer) expTime
      return 0

incrementPoolRadiusStep ::
  ( Redis.HedisFlow m r
  ) =>
  Id DSR.SearchRequest ->
  m ()
incrementPoolRadiusStep searchReqId = do
  res <- Redis.withCrossAppRedis $ Redis.incr (poolRadiusStepKey searchReqId)
  logInfo $ "Increment radius step to " <> show res <> "."
  return ()
