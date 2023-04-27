{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.LocationUpdate.Processor
  ( processLocationData,
  )
where

import qualified Consumer.AvailabilityTime.Types as T
import qualified Consumer.LocationUpdate.Types as LT
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Time
import Data.Time.Clock.POSIX
import Environment
import EulerHS.Prelude hiding (toStrict)
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Logging (logInfo)

processLocationData :: T.LocationUpdates -> T.DriverId -> Flow ()
processLocationData T.LocationUpdates {..} driverId = do
  let newUpdatedAt = ts
  logInfo $ "driver updated time " <> show newUpdatedAt <> driverId
  let encodedVal = A.encode $ createDriverIdTokenKey driverId dt
  Redis.zadd "driver-last-location-update" [(utcToDouble newUpdatedAt, BSL.toStrict encodedVal)]

utcToDouble :: UTCTime -> Double
utcToDouble = realToFrac . utcTimeToPOSIXSeconds

createDriverIdTokenKey :: T.DriverId -> Maybe FCM.FCMRecipientToken -> LT.DriverIdTokenKey
createDriverIdTokenKey driverId dt =
  LT.DriverIdTokenKey
    { driverId = driverId,
      deviceToken = dt
    }
