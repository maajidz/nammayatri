{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.TransporterConfig
  ( findByMerchantId,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.TransporterConfig
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.TransporterConfig as Queries

findByMerchantId :: (CacheFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> m (Maybe TransporterConfig)
findByMerchantId id =
  Hedis.get (makeMerchantIdKey id) >>= \case
    Just a -> return . Just $ coerce @(TransporterConfigD 'Unsafe) @TransporterConfig a
    Nothing -> flip whenJust cacheTransporterConfig /=<< Queries.findByMerchantId id

cacheTransporterConfig :: (CacheFlow m r) => TransporterConfig -> m ()
cacheTransporterConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantIdKey cfg.merchantId
  Hedis.setExp merchantIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey id = "CachedQueries:TransporterConfig:MerchantId-" <> id.getId
