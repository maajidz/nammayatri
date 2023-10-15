{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Coins.CoinPlan where

import Domain.Types.Coins.CoinPlan
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Coins.CoinPlan as Queries

getCoinPlanDetails :: (MonadFlow m, CacheFlow m r) => Id CoinPlan -> m (Maybe CoinPlan)
getCoinPlanDetails (Id coinPlanId) =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeCoinPlanIdKey (Id coinPlanId)) >>= \case
    Just a -> pure a
    Nothing -> cacheByCoinPlanId (Id coinPlanId) /=<< Queries.getCoinPlanDetails (Id coinPlanId)

cacheByCoinPlanId :: (CacheFlow m r) => Id CoinPlan -> Maybe CoinPlan -> m ()
cacheByCoinPlanId (Id coinPlanId) coinPlan = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCoinPlanIdKey (Id coinPlanId)) coinPlan expTime

----- CACHED KEYS -----
fetchAllCoinPlan :: (CacheFlow m r, MonadFlow m) => Id DM.Merchant -> m [CoinPlan]
fetchAllCoinPlan (Id merchantId) =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeAllPlanKey (Id merchantId)) >>= \case
    Just a -> pure a
    Nothing -> cacheAllPlan (Id merchantId) /=<< Queries.getCoinPlans (Id merchantId)

cacheAllPlan :: (CacheFlow m r) => Id DM.Merchant -> [CoinPlan] -> m ()
cacheAllPlan merchantId plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeAllPlanKey merchantId) plans expTime

makeAllPlanKey :: Id DM.Merchant -> Text
makeAllPlanKey (Id merchantId) = "driver-offer:CachedQueries:CoinPlan:CoinPlanId-ALL:MerchantId-" <> merchantId

makeCoinPlanIdKey :: Id CoinPlan -> Text
makeCoinPlanIdKey (Id coinPlanId) = "driver-offer:CachedQueries:CoinPlan:CoinPlanId-" <> coinPlanId
