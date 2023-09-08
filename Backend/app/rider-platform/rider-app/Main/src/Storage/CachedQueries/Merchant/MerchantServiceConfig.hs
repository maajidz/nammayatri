{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceConfig
  ( findByMerchantOperatingCityIdAndService,
    clearCache,
    cacheMerchantServiceConfig,
    upsertMerchantServiceConfig,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Merchant.MerchantServiceConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.MerchantServiceConfig as Queries

findByMerchantOperatingCityIdAndService :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantOperatingCityIdAndService id serviceName =
  Hedis.safeGet (makeMerchantOperatingCityIdAndServiceKey id serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheMerchantServiceConfig /=<< Queries.findByMerchantOperatingCityIdAndService id serviceName

cacheMerchantServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheMerchantServiceConfig merchantServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOperatingCityIdAndServiceKey merchantServiceConfig.merchantOperatingCityId (getServiceName merchantServiceConfig)
  Hedis.setExp idKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) merchantServiceConfig) expTime

makeMerchantOperatingCityIdAndServiceKey :: Id MerchantOperatingCity -> ServiceName -> Text
makeMerchantOperatingCityIdAndServiceKey id serviceName = "CachedQueries:MerchantServiceConfig:MerchantId-" <> id.getId <> ":ServiceName-" <> show serviceName

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> ServiceName -> m ()
clearCache merchanOperatingCityId serviceName = do
  Hedis.del (makeMerchantOperatingCityIdAndServiceKey merchanOperatingCityId serviceName)

upsertMerchantServiceConfig :: MonadFlow m => MerchantServiceConfig -> m ()
upsertMerchantServiceConfig = Queries.upsertMerchantServiceConfig
