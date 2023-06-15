{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy
  ( findById,
    clearCache,
    update,
    clearCacheById,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import qualified EulerHS.Language as L
import Kernel.Prelude
-- import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy as Queries

-- import EulerHS.KVConnector.Types

-- getUpdatedFarePolicy :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Maybe Vehicle.Variant -> Meters -> FarePolicy -> m FarePolicy
-- getUpdatedFarePolicy mId varId distance farePolicy = do
--   restrictedPolicy <-
--     case varId of
--       Just var -> findRestrictedFareListByMerchantAndVehicle mId var
--       Nothing -> findRestrictedFareListByMerchant mId
--   pure $ maybe farePolicy (updateMaxExtraFare farePolicy) (restrictedFair restrictedPolicy)
--   where
--     updateMaxExtraFare FarePolicy {..} maxFee = FarePolicy {driverExtraFeeBounds = driverExtraFeeBounds <&> \b -> b {maxFee}, ..}
--     restrictedFair fares =
--       case find (\fare -> fare.minTripDistance <= distance) fares of
--         Just fare -> Just (fare.driverMaxExtraFare)
--         Nothing -> Nothing

findById :: (CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
    Nothing -> flip whenJust cacheFarePolicy /=<< Queries.findById id

cacheFarePolicy :: (CacheFlow m r) => FarePolicy -> m ()
cacheFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @FarePolicy @(FarePolicyD 'Unsafe) fp) expTime

makeIdKey :: Id FarePolicy -> Text
makeIdKey id = "driver-offer:CachedQueries:FarePolicy:Id-" <> id.getId

-- Call it after any update
clearCache :: HedisFlow m r => FarePolicy -> m ()
clearCache fp = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fp.id)

clearCacheById :: HedisFlow m r => Id FarePolicy -> m ()
clearCacheById fid = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fid)

update :: (L.MonadFlow m, MonadTime m) => FarePolicy -> m ()
update = Queries.update
