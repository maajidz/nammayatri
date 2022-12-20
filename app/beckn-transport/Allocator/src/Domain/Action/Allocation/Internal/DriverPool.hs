module Domain.Action.Allocation.Internal.DriverPool
  ( isBatchNumExceedLimit,
    getNextDriverPoolBatch,
    cleanupDriverPoolBatches,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Action.Allocation.Internal.DriverPool.Config
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.TransporterConfig as STConf
import Environment (Flow)
import SharedLogic.DriverPool (calculateDriverPool)
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.TransporterConfig as QTConf
import qualified Storage.Queries.Booking as QBooking
import Tools.Metrics

isBatchNumExceedLimit ::
  ( Redis.HedisFlow m r,
    HasDriverPoolBatchesConfig r
  ) =>
  Id SRB.Booking ->
  m Bool
isBatchNumExceedLimit bookingId = do
  maxNumberOfBatches <- asks (.driverPoolBatchesCfg.maxNumberOfBatches)
  currentBatchNum <- getPoolBatchNum bookingId
  return $ currentBatchNum >= maxNumberOfBatches

driverPoolKey :: Id SRB.Booking -> Text
driverPoolKey bookingId = "DriverPool:BookingId-" <> bookingId.getId

driverPoolBatchKey :: Id SRB.Booking -> PoolBatchNum -> Text
driverPoolBatchKey bookingId batchNum = driverPoolKey bookingId <> ":BatchNum-" <> show batchNum

prepareDriverPoolBatch ::
  ( EncFlow m r,
    HasCacheConfig r,
    CoreMetrics m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r,
    HasDriverPoolBatchesConfig r
  ) =>
  Id SRB.Booking ->
  PoolBatchNum ->
  m [DriverPoolResult]
prepareDriverPoolBatch bookingId batchNum = withLogTag ("BatchNum-" <> show batchNum) $ do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  previousBatchesDrivers <- getPreviousBatchesDrivers
  logDebug $ "PreviousBatchesDrivers-" <> show previousBatchesDrivers
  prepareDriverPoolBatch' booking previousBatchesDrivers
  where
    prepareDriverPoolBatch' booking previousBatchesDrivers = do
      radiusStep <- getPoolRadiusStep bookingId
      driverPool <- calcDriverPool booking radiusStep
      logDebug $ "DriverPool-" <> show driverPool
      sortedDriverPool <- sortFunction driverPool
      logDebug $ "SortedDriverPool-" <> show sortedDriverPool
      let onlyNewDriversPool = filter (\dpr -> dpr.driverId `notElem` previousBatchesDrivers) sortedDriverPool
      logDebug $ "OnlyNewDriversPool-" <> show onlyNewDriversPool
      driverPoolBatch <- getBatch onlyNewDriversPool
      logDebug $ "DriverPoolBatch-" <> show driverPoolBatch
      batchSize <- asks (.driverPoolBatchesCfg.driverBatchSize)
      if length driverPoolBatch < batchSize
        then do
          isAtMaxRadiusStep' <- isAtMaxRadiusStep booking radiusStep
          if isAtMaxRadiusStep'
            then do
              filledBatch <- fillBatch batchSize sortedDriverPool driverPoolBatch
              logDebug $ "FilledDriverPoolBatch-" <> show filledBatch
              cacheBatch filledBatch
              return filledBatch
            else do
              incrementPoolRadiusStep bookingId
              prepareDriverPoolBatch' booking previousBatchesDrivers
        else do
          cacheBatch driverPoolBatch
          return driverPoolBatch

    calcDriverPool ::
      ( EncFlow m r,
        HasCacheConfig r,
        CoreMetrics m,
        EsqDBFlow m r,
        Redis.HedisFlow m r,
        HasDriverPoolConfig r
      ) =>
      SRB.Booking ->
      PoolRadiusStep ->
      m [DriverPoolResult]
    calcDriverPool booking radiusStep = do
      let vehicleVariant = booking.vehicleVariant
          merchantId = booking.providerId
      let pickupLoc = booking.fromLocation
          fareProductType = SRB.getFareProductType booking.bookingDetails
      calculateDriverPool pickupLoc merchantId (Just vehicleVariant) fareProductType (Just radiusStep)
    getBatch driverPool = do
      batchSize <- asks (.driverPoolBatchesCfg.driverBatchSize)
      return $ take batchSize driverPool
    fillBatch batchSize driverPool batch = do
      let batchDriverIds = batch <&> (.driverId)
      let driversToFillBatch = take (batchSize - length batch) $ filter (\dpr -> dpr.driverId `notElem` batchDriverIds) driverPool
      return $ batch <> driversToFillBatch
    cacheBatch batch = do
      logDebug $ "Caching batch-" <> show batch
      Redis.setExp (driverPoolBatchKey bookingId batchNum) batch (60 * 10)
    isAtMaxRadiusStep booking radiusStep = do
      let merchantId = booking.providerId
      minRadius :: Double <-
        QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "min_radius")
          >>= maybe
            (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
            radiusFromTransporterConfig
      maxRadius :: Double <-
        QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "max_radius")
          >>= maybe
            (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
            radiusFromTransporterConfig
      radiusStepSize :: Double <-
        QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "radius_step_size")
          >>= maybe
            (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
            radiusFromTransporterConfig
      let maxRadiusStep = ceiling $ (maxRadius - minRadius) / radiusStepSize
      return $ maxRadiusStep <= radiusStep
    radiusFromTransporterConfig conf =
      fromMaybeM (InternalError "Value is not a number.")
        . readMaybe
        . toString
        $ conf.value

    getPreviousBatchesDrivers = do
      batches <- forM [0 .. (batchNum - 1)] \num -> do
        getDriverPoolBatch bookingId num
      return $ (.driverId) <$> concat batches
    sortFunction driverPool =
      return driverPool -- it's random order by default

getDriverPoolBatch ::
  ( Redis.HedisFlow m r
  ) =>
  Id SRB.Booking ->
  PoolBatchNum ->
  m [DriverPoolResult]
getDriverPoolBatch bookingId batchNum = do
  Redis.get (driverPoolBatchKey bookingId batchNum)
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

poolBatchNumKey :: Id SRB.Booking -> Text
poolBatchNumKey bookingId = "Allocator:PoolBatchNum:BookingId-" <> bookingId.getId

poolRadiusStepKey :: Id SRB.Booking -> Text
poolRadiusStepKey bookingId = "Allocator:PoolRadiusStep:BookingId-" <> bookingId.getId

cleanupDriverPoolBatches ::
  ( Redis.HedisFlow m r
  ) =>
  Id SRB.Booking ->
  m ()
cleanupDriverPoolBatches bookingId = do
  Redis.delByPattern (driverPoolKey bookingId <> "*")
  Redis.del (poolRadiusStepKey bookingId)
  Redis.del (poolBatchNumKey bookingId)
  logInfo "Cleanup redis."

getNextDriverPoolBatch :: Id SRB.Booking -> Flow [DriverPoolResult]
getNextDriverPoolBatch bookingId = withLogTag "getNextDriverPoolBatch" do
  batchNum <- getPoolBatchNum bookingId
  incrementBatchNum bookingId
  prepareDriverPoolBatch bookingId batchNum

getPoolBatchNum :: (Redis.HedisFlow m r) => Id SRB.Booking -> m PoolBatchNum
getPoolBatchNum bookingId = do
  res <- Redis.get (poolBatchNumKey bookingId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.setExp (poolBatchNumKey bookingId) (0 :: Integer) expTime
      return 0

incrementBatchNum ::
  ( Redis.HedisFlow m r
  ) =>
  Id SRB.Booking ->
  m ()
incrementBatchNum bookingId = do
  res <- Redis.incr (poolBatchNumKey bookingId)
  logInfo $ "Increment batch num to " <> show res <> "."
  return ()

getPoolRadiusStep :: (Redis.HedisFlow m r) => Id SRB.Booking -> m PoolRadiusStep
getPoolRadiusStep bookingId = do
  res <- Redis.get (poolRadiusStepKey bookingId)
  case res of
    Just i -> return i
    Nothing -> do
      let expTime = 600
      Redis.setExp (poolRadiusStepKey bookingId) (0 :: Integer) expTime
      return 0

incrementPoolRadiusStep ::
  ( Redis.HedisFlow m r
  ) =>
  Id SRB.Booking ->
  m ()
incrementPoolRadiusStep bookingId = do
  res <- Redis.incr (poolRadiusStepKey bookingId)
  logInfo $ "Increment radius step to " <> show res <> "."
  return ()
