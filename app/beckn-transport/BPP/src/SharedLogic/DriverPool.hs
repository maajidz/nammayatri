module SharedLogic.DriverPool
  ( calculateDriverPool,
    module Reexport,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.TransporterConfig as STConf
import qualified Domain.Types.Vehicle as SV
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.TransporterConfig as QTConf
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Maps as Google
import qualified Tools.Maps as Maps
import Tools.Metrics

calculateDriverPool ::
  ( EncFlow m r,
    HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverPoolConfig r,
    CoreMetrics m,
    HasCoordinates a
  ) =>
  a ->
  Id DM.Merchant ->
  Maybe SV.Variant ->
  SFP.FareProductType ->
  Maybe PoolRadiusStep ->
  m [DriverPoolResult]
calculateDriverPool pickup merchantId variant fareProductType mRadiusStep = do
  let pickupLatLong = getCoordinates pickup
  radius <- getRadius
  mbDriverPositionInfoExpiry <- asks (.driverPoolCfg.driverPositionInfoExpiry)
  nearestDriversResult <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        pickupLatLong
        radius
        merchantId
        variant
        fareProductType
        mbDriverPositionInfoExpiry
  case nearestDriversResult of
    [] -> pure []
    (a : xs) -> do
      approxDriverPool' <- buildDriverPoolResults merchantId pickupLatLong (a :| xs)
      filterOutDriversWithDistanceAboveThreshold radius approxDriverPool'
  where
    getRadius = do
      maxRadius <-
        QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "max_radius")
          >>= maybe
            (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
            radiusFromTransporterConfig
      case mRadiusStep of
        Just radiusStep -> do
          minRadius <-
            QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "min_radius")
              >>= maybe
                (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
                radiusFromTransporterConfig
          radiusStepSize <-
            QTConf.findValueByMerchantIdAndKey merchantId (STConf.ConfigKey "radius_step_size")
              >>= maybe
                (fromIntegral <$> asks (.driverPoolCfg.defaultRadiusOfSearch))
                radiusFromTransporterConfig
          return $ min (minRadius + radiusStepSize * radiusStep) maxRadius
        Nothing -> return maxRadius

    radiusFromTransporterConfig conf =
      fromMaybeM (InternalError "Value is not a number.")
        . readMaybe
        . toString
        $ conf.value

buildDriverPoolResults ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  LatLong ->
  NonEmpty QP.NearestDriversResult ->
  m (NonEmpty DriverPoolResult)
buildDriverPoolResults orgId pickup ndResults = do
  distDurs <-
    Maps.getDistances orgId $
      Maps.GetDistancesReq
        { origins = ndResults,
          destinations = pickup :| [],
          travelMode = Just Maps.CAR
        }
  return $ mkDriverPoolResult <$> distDurs
  where
    mkDriverPoolResult distDur = do
      let QP.NearestDriversResult {..} = distDur.origin
      DriverPoolResult
        { distanceToPickup = distDur.distance,
          durationToPickup = distDur.duration,
          ..
        }

filterOutDriversWithDistanceAboveThreshold ::
  ( EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Int ->
  NonEmpty DriverPoolResult ->
  m [DriverPoolResult]
filterOutDriversWithDistanceAboveThreshold threshold driverPoolResults = do
  pure $ NE.filter filterFunc driverPoolResults
  where
    filterFunc drPoolRes = drPoolRes.distanceToPickup <= fromIntegral threshold
