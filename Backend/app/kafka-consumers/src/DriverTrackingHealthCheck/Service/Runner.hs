{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module DriverTrackingHealthCheck.Service.Runner where

import Consumer.LocationUpdate.Types (DriverIdTokenKey)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import Data.Time.Clock.POSIX hiding (getCurrentTime)
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as SP
import qualified DriverTrackingHealthCheck.API as HC
import Environment (Flow, HealthCheckAppCfg)
import Kernel.External.Encryption (decrypt)
import Kernel.External.FCM.Types (FCMNotificationType (TRIGGER_SERVICE))
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.SMS.MyValueFirst.Flow as SF
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (lPush, rPop)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error (PersonError (PersonFieldNotPresent, PersonNotFound))
import Kernel.Types.Id (cast)
import Kernel.Utils.Common
import Kernel.Utils.Service
import qualified Storage.Queries.DriverInformation as DrInfo
import qualified Storage.Queries.Person as SQP
import Tools.Notifications

driverTrackingHealthcheckService :: HealthCheckAppCfg -> Flow ()
driverTrackingHealthcheckService heathCheckConfig = withLogTag "driverTrackingHealthcheckService" do
  driverLastLocationUpdateCheckService heathCheckConfig
  driverDevicePingService heathCheckConfig
  driverMakingInactiveService heathCheckConfig

driverLastLocationUpdateCheckService :: HealthCheckAppCfg -> Flow ()
driverLastLocationUpdateCheckService healthCheckAppCfg = startService "driverLastLocationUpdateCheckService" $ withRandomId do
  let locationDelay = healthCheckAppCfg.driverAllowedDelayForLocationUpdateInSec
      serviceInterval = healthCheckAppCfg.driverLocationHealthCheckIntervalInSec
  withLock "driver-tracking-healthcheck" $ measuringDurationToLog INFO "driverLastLocationUpdateCheckService" do
    now <- getCurrentTime
    HC.iAmAlive
    driverDetails <- getAllDrivers locationDelay now
    flip map driverDetails \case
      (driverId, Nothing) -> Left driverId
      (driverId, Just token) -> Right (driverId, token)
      & partitionEithers
      & \(noTokenIds, driversToPing) -> do
        unless (null noTokenIds) $ logPretty ERROR "Active drivers with no token" noTokenIds
        case nonEmpty driversToPing of
          Just driversWithToken -> do
            lPush redisKey driversWithToken
            logPretty INFO ("Drivers to ping: " <> show (length driversWithToken)) driversWithToken
          Nothing -> log INFO "No drivers to ping"
  threadDelay (secondsToMcs serviceInterval).getMicroseconds

redisKey :: Text
redisKey = "beckn:driver-tracking-healthcheck:drivers-to-ping"

driverDevicePingService :: HealthCheckAppCfg -> Flow ()
driverDevicePingService healthCheckAppCfg = startService "driverDevicePingService" do
  HC.iAmAlive
  rPop redisKey >>= flip whenJust \(driverId, token) ->
    withLogTag driverId.getId do
      log INFO "Ping driver"
      driver <- SQP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      notifyDevice driver.merchantId TRIGGER_SERVICE "You were inactive" "Please check the app" driverId (Just token)
  pure healthCheckAppCfg.notificationMinDelay >>= threadDelay . (.getMicroseconds)

driverMakingInactiveService :: HealthCheckAppCfg -> Flow ()
driverMakingInactiveService healthCheckAppCfg = startService "driverMakingInactiveService" $ withRandomId do
  let delay = healthCheckAppCfg.driverInactiveDelay
  withLock "driver-tracking-healthcheck" $ measuringDurationToLog INFO "driverMakingInactiveService" do
    now <- getCurrentTime
    HC.iAmAlive
    drivers <- DrInfo.getDriversWithOutdatedLocationsToMakeInactive (negate (fromIntegral delay) `addUTCTime` now)
    logPretty INFO ("Drivers to make inactive: " <> show (length drivers)) ((.id) <$> drivers)
    mapM_ fetchPersonIdAndMobileNumber drivers
  threadDelay (secondsToMcs delay).getMicroseconds
  where
    fetchPersonIdAndMobileNumber :: SP.Person -> Flow ()
    fetchPersonIdAndMobileNumber driver = withLogTag ("driverId_" <> driver.id.getId) do
      mobileNumber' <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      log INFO "Make driver inactive"
      Esq.runTransaction $
        DrInfo.updateActivity (cast driver.id) False (Just DI.OFFLINE)

      let smsCfg = healthCheckAppCfg.smsCfg
          driverInactiveSmsTemplate = healthCheckAppCfg.driverInactiveSmsTemplate

      fork "smsServiceToMarkDriverInactive" $
        SF.sendSms smsCfg driverInactiveSmsTemplate (countryCode <> mobileNumber')
          >>= SF.checkSmsResult

withLock :: (Redis.HedisFlow m r, MonadMask m) => Text -> m () -> m ()
withLock serviceName func =
  Redis.withLockRedis key 10 (func `catch` (logError . makeLogSomeException))
  where
    key = "beckn:" <> serviceName <> ":lock"

getAllDrivers :: (Redis.HedisFlow m r, MonadReader r m) => Seconds -> UTCTime -> m [(Text, Maybe FCM.FCMRecipientToken)]
getAllDrivers locationDelay now = do
  let presentTime = negate (fromIntegral locationDelay) `addUTCTime` now
  redisRes <- Redis.withCrossAppRedis $ Redis.zrangebyscore "driver-last-location-update" 0 $ utcToDouble presentTime
  let mbDecodedVal = map decode redisRes
  let decodedVal = catMaybes mbDecodedVal
  let resTuple = map (\val -> (val.driverId, val.deviceToken)) decodedVal
  pure resTuple
  where
    decode :: BS.ByteString -> Maybe DriverIdTokenKey
    decode val = do
      let res = A.decode $ BSL.fromStrict val
      res

utcToDouble :: UTCTime -> Double
utcToDouble = realToFrac . utcTimeToPOSIXSeconds
