module API.UI.Search
  ( SearchReq (..),
    SearchRes (..),
    DOneWaySearch.OneWaySearchReq (..),
    DRentalSearch.RentalSearchReq (..),
    DSearchCommon.SearchReqLocation (..),
    API,
    handler,
  )
where

import qualified Beckn.External.Slack.Flow as SF
import Beckn.External.Slack.Types (SlackConfig)
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Storage.Hedis (HedisFlow)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch
import Beckn.Streaming.MonadProducer
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.Common
import Beckn.Utils.SlidingWindowLimiter
import qualified Core.ACL.Metro.Search as MetroACL
import qualified Core.ACL.Search as TaxiACL
import Data.Aeson
import Data.OpenApi
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.Person as Person
import Domain.Types.SearchRequest (SearchRequest)
import Environment
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.PublicTransport as PublicTransport
import Storage.CachedQueries.CacheConfig
import Tools.Auth
import qualified Tools.JSON as J
import Tools.Metrics

-------- Search Flow --------

type API =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] SearchRes

handler :: FlowServer API
handler = search

data SearchReq = OneWaySearch DOneWaySearch.OneWaySearchReq | RentalSearch DRentalSearch.RentalSearchReq
  deriving (Generic, Show)

instance ToJSON SearchReq where
  toJSON = genericToJSON fareProductOptions

instance FromJSON SearchReq where
  parseJSON = genericParseJSON fareProductOptions

instance ToSchema SearchReq where
  declareNamedSchema = genericDeclareNamedSchema fareProductSchemaOptions

fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = J.fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductSchemaOptions :: OpenApi.SchemaOptions
fareProductSchemaOptions =
  OpenApi.defaultSchemaOptions
    { OpenApi.sumEncoding = J.fareProductTaggedObject,
      OpenApi.constructorTagModifier = fareProductConstructorModifier
    }

newtype SearchRes = SearchRes
  { searchId :: Id SearchRequest
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWaySearch" -> "ONE_WAY"
  "RentalSearch" -> "RENTAL"
  x -> x

search :: Id Person.Person -> SearchReq -> FlowHandler SearchRes
search personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  searchId <- case req of
    OneWaySearch oneWay -> oneWaySearch personId oneWay
    RentalSearch rental -> rentalSearch personId rental
  return $ SearchRes searchId

oneWaySearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds, "gatewayUrl" ::: BaseUrl],
    HasBAPMetrics m r,
    MonadProducer PublicTransportSearch m
  ) =>
  Id Person.Person ->
  DOneWaySearch.OneWaySearchReq ->
  m (Id SearchRequest)
oneWaySearch personId req = do
  dSearchRes <- DOneWaySearch.oneWaySearch personId req
  fork "search cabs" . withRetry $ do
    becknTaxiReq <- TaxiACL.buildOneWaySearchReq dSearchRes
    void $ CallBPP.search dSearchRes.gatewayUrl becknTaxiReq
  fork "search metro" . withRetry $ do
    becknMetroReq <- MetroACL.buildSearchReq dSearchRes
    CallBPP.searchMetro becknMetroReq
  fork "search public-transport" $ PublicTransport.sendPublicTransportSearchRequest personId dSearchRes
  return dSearchRes.searchId

rentalSearch ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds, "gatewayUrl" ::: BaseUrl],
    HasBAPMetrics m r
  ) =>
  Id Person.Person ->
  DRentalSearch.RentalSearchReq ->
  m (Id SearchRequest)
rentalSearch personId req = do
  dSearchRes <- DRentalSearch.rentalSearch personId req
  fork "search rental" . withRetry $ do
    -- do we need fork here?
    becknReq <- TaxiACL.buildRentalSearchReq dSearchRes
    void $ CallBPP.search dSearchRes.gatewayUrl becknReq
  pure $ dSearchRes.searchId

checkSearchRateLimit ::
  ( Redis.HedisFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text],
    MonadTime m
  ) =>
  Id Person.Person ->
  m ()
checkSearchRateLimit personId = do
  let key = searchHitsCountKey personId
  hitsLimit <- asks (.searchRateLimitOptions.limit)
  limitResetTimeInSec <- asks (.searchRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    msgTemplate <- asks (.searchLimitExceedNotificationTemplate)
    let message = T.replace "{#cust-id#}" (getId personId) msgTemplate
    _ <- SF.postMessage message
    throwError $ HitsLimitError limitResetTimeInSec

searchHitsCountKey :: Id Person.Person -> Text
searchHitsCountKey personId = "BAP:Ride:search:" <> getId personId <> ":hitsCount"
