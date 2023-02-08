module Lib.LocationUpdates
  ( module Reexport,
    whenWithLocationUpdatesLock,
    buildRideInterpolationHandler,
  )
where

import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant.MerchantServiceConfig as DOSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import "location-updates" Lib.LocationUpdates as Reexport
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QOMC
import qualified Storage.Queries.Ride as QRide
import Tools.Error

buildRideInterpolationHandler :: Id Merchant -> Bool -> Flow (RideInterpolationHandler Person.Person Flow)
buildRideInterpolationHandler orgId isEndRide = do
  orgMapsConfig <- QOMC.findByMerchantId orgId >>= fromMaybeM (MerchantServiceUsageConfigNotFound orgId.getId)
  orgMapsServiceConfig <-
    QOMSC.findByMerchantIdAndService orgId (DOSC.MapsService orgMapsConfig.snapToRoad)
      >>= fromMaybeM (MerchantServiceConfigNotFound orgId.getId orgMapsConfig.snapToRoad)
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig cfg ->
      return $
        mkRideInterpolationHandler isEndRide cfg $
          \driverId dist -> Esq.runTransaction $ QRide.updateDistance driverId dist
    _ -> throwError $ InternalError "Unknown Service Config"

whenWithLocationUpdatesLock :: (HedisFlow m r, MonadMask m) => Id DP.Person -> m () -> m ()
whenWithLocationUpdatesLock driverId f = do
  redisLockDriverId <- Redis.tryLockRedis lockKey 60
  if redisLockDriverId
    then do
      logDebug $ lockKey <> " Locked"
      finally
        f
        ( do
            Redis.unlockRedis lockKey
            logDebug $ "DriverId: " <> show driverId <> " Unlocked"
        )
    else do
      logDebug $ lockKey <> " unable to get lock"
      throwError (HitsLimitError 5)
  where
    lockKey = "DriverLocationUpdate:DriverId-" <> driverId.getId
