module Beckn.Utils.Registry
  ( registryFetch,
    Beckn.Utils.Registry.registryLookup,
    whitelisting,
    withSubscriberCache,
  )
where

import Beckn.Prelude
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Types.Registry
import qualified Beckn.Types.Registry.API as API
import qualified Beckn.Types.Registry.Routes as Registry
import Beckn.Utils.Common
import Data.Generics.Labels ()
import qualified EulerHS.Types as T

registryLookup ::
  ( MonadReader r m,
    MonadFlow m,
    CoreMetrics m,
    HasInConfig r c "registryUrl" BaseUrl
  ) =>
  SimpleLookupRequest ->
  m (Maybe Subscriber)
registryLookup request =
  registryFetch (toLookupReq request)
    >>= \case
      [subscriber] ->
        pure $ Just subscriber
      _subscriber : _subscribers ->
        throwError $ InternalError "Multiple subscribers returned for a unique key."
      [] -> pure Nothing
  where
    toLookupReq SimpleLookupRequest {..} =
      API.emptyLookupRequest
        { API.unique_key_id = Just unique_key_id,
          API.subscriber_id = Just subscriber_id
        }

registryFetch ::
  ( MonadReader r m,
    MonadFlow m,
    CoreMetrics m,
    HasInConfig r c "registryUrl" BaseUrl
  ) =>
  API.LookupRequest ->
  m [Subscriber]
registryFetch request = do
  registryUrl <- askConfig (.registryUrl)
  callAPI registryUrl (T.client Registry.lookupAPI request) "lookup"
    >>= fromEitherM (ExternalAPICallError (Just "REGISTRY_CALL_ERROR") registryUrl)

whitelisting ::
  (MonadThrow m, Log m) =>
  (Text -> m Bool) ->
  Maybe Subscriber ->
  m (Maybe Subscriber)
whitelisting p = maybe (pure Nothing) \sub -> do
  unlessM (p sub.subscriber_id) . throwError . InvalidRequest $
    "Not whitelisted subscriber " <> sub.subscriber_id
  pure (Just sub)

withSubscriberCache ::
  ( MonadTime m,
    CacheEx Subscriber m
  ) =>
  (CacheKey Subscriber -> m (Maybe Subscriber)) ->
  CacheKey Subscriber ->
  m (Maybe Subscriber)
withSubscriberCache getData key = do
  now <- getCurrentTime
  caching (getTtl now) getData key
  where
    getTtl now Subscriber {..} =
      nominalDiffTimeToSeconds . fromMaybe (5 * 60) $ valid_until <&> (`diffUTCTime` now)
