{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Plan where

import Domain.Types.Merchant
import Domain.Types.Plan
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Plan as Queries

findByIdAndPaymentMode :: (CacheFlow m r, L.MonadFlow m) => Id Plan -> PaymentMode -> m (Maybe Plan)
findByIdAndPaymentMode (Id planId) paymentMode =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makePlanIdAndPaymentModeKey (Id planId) paymentMode) >>= \case
    Just a -> pure a
    Nothing -> cacheByIdAndPaymentMode (Id planId) paymentMode /=<< Queries.findByIdAndPaymentMode (Id planId) paymentMode

cacheByIdAndPaymentMode :: (CacheFlow m r) => Id Plan -> PaymentMode -> (Maybe Plan) -> m ()
cacheByIdAndPaymentMode (Id planId) paymentMode plan = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makePlanIdAndPaymentModeKey (Id planId) paymentMode) plan expTime

------------------- -----------------------
findByMerchantId :: (CacheFlow m r, L.MonadFlow m) => Id Merchant -> m [Plan]
findByMerchantId (Id merchantId) =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey (Id merchantId)) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantId (Id merchantId) /=<< Queries.findByMerchantId (Id merchantId)

cacheByMerchantId :: (CacheFlow m r) => Id Merchant -> [Plan] -> m ()
cacheByMerchantId (Id merchantId) plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdKey (Id merchantId)) plans expTime

------------------- -----------------------
findByMerchantIdAndPaymentMode :: (CacheFlow m r, L.MonadFlow m) => Id Merchant -> PaymentMode -> m [Plan]
findByMerchantIdAndPaymentMode (Id merchantId) paymentMode =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdAndPaymentModeKey (Id merchantId) paymentMode) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantIdAndPaymentMode (Id merchantId) paymentMode /=<< Queries.findByMerchantIdAndPaymentMode (Id merchantId) paymentMode

cacheByMerchantIdAndPaymentMode :: (CacheFlow m r) => Id Merchant -> PaymentMode -> [Plan] -> m ()
cacheByMerchantIdAndPaymentMode (Id merchantId) paymentMode plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdAndPaymentModeKey (Id merchantId) paymentMode) plans expTime

------------------- -----------------------
fetchAllPlan :: (CacheFlow m r, L.MonadFlow m) => m [Plan]
fetchAllPlan =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeAllPlanKey) >>= \case
    Just a -> pure a
    Nothing -> cacheAllPlan /=<< Queries.fetchAllPlan

cacheAllPlan :: (CacheFlow m r) => [Plan] -> m ()
cacheAllPlan plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeAllPlanKey) plans expTime

makeAllPlanKey :: Text
makeAllPlanKey = "driver-offer:CachedQueries:Plan:PlanId-ALL"

makePlanIdAndPaymentModeKey :: Id Plan -> PaymentMode -> Text
makePlanIdAndPaymentModeKey id paymentMode = "driver-offer:CachedQueries:Plan:PlanId-" <> id.getId <> ":PaymentMode-" <> (show paymentMode)

makeMerchantIdAndPaymentModeKey :: Id Merchant -> PaymentMode -> Text
makeMerchantIdAndPaymentModeKey merchantId paymentMode = "driver-offer:CachedQueries:Plan:MerchantId-" <> merchantId.getId <> ":PaymentMode-" <> (show paymentMode)

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey merchantId = "driver-offer:CachedQueries:Plan:MerchantId-" <> merchantId.getId
