module Environment
  ( Env,
    FlowHandler,
    FlowServer,
    Flow,
    AppCfg (),
    AppEnv (..),
    BAPs (..),
    HasBapInfo,
    RideConfig (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption (EncTools)
import Kernel.External.Exotel.Types (ExotelCfg)
import Kernel.External.Infobip.Types (InfoBIPConfig, WebengageConfig)
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis as Redis
import Kernel.Storage.Hedis.AppPrefixes (riderAppPrefix)
import Kernel.Types.App
import Kernel.Types.Cache
import Kernel.Types.Common
import Kernel.Types.Credentials (PrivateKey)
import Kernel.Types.Flow
import Kernel.Types.Id (ShortId (..))
import Kernel.Types.Registry
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.App (getPodName)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Servant.SignatureAuth
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.BlackListOrg (findByShortId)
import Storage.CachedQueries.CacheConfig
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    smsCfg :: SmsConfig,
    infoBIPCfg :: InfoBIPConfig,
    webengageCfg :: WebengageConfig,
    otpSmsTemplate :: Text,
    port :: Int,
    metricsPort :: Int,
    hostName :: Text,
    bapSelfIds :: BAPs Text,
    bapSelfURIs :: BAPs BaseUrl,
    bapSelfUniqueKeyIds :: BAPs Text,
    searchRequestExpiry :: Maybe Seconds,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    metricsSearchDurationTimeout :: Seconds,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    searchRateLimitOptions :: APIRateLimitOptions,
    slackCfg :: SlackConfig,
    searchLimitExceedNotificationTemplate :: Text,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    registryUrl :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    disableSignatureAuth :: Bool,
    gatewayUrl :: BaseUrl,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    selfUIUrl :: BaseUrl,
    rideCfg :: RideConfig,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig
  }
  deriving (Generic, FromDhall)

-- TODO coreVersion should be hardcoded in spec, because we can't change coreVersion without changing code
data AppEnv = AppEnv
  { smsCfg :: SmsConfig,
    infoBIPCfg :: InfoBIPConfig,
    webengageCfg :: WebengageConfig,
    otpSmsTemplate :: Text,
    hostName :: Text,
    bapSelfIds :: BAPs Text,
    bapSelfURIs :: BAPs BaseUrl,
    searchRequestExpiry :: Maybe Seconds,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    loggerConfig :: LoggerConfig,
    googleTranslateUrl :: BaseUrl,
    googleTranslateKey :: Text,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    searchRateLimitOptions :: APIRateLimitOptions,
    slackCfg :: SlackConfig,
    searchLimitExceedNotificationTemplate :: Text,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    authTokenCacheExpiry :: Seconds,
    registryUrl :: BaseUrl,
    signingKey :: PrivateKey,
    signatureExpiry :: Seconds,
    disableSignatureAuth :: Bool,
    gatewayUrl :: BaseUrl,
    encTools :: EncTools,
    selfUIUrl :: BaseUrl,
    hedisEnv :: HedisEnv,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    isShuttingDown :: TMVar (),
    bapMetrics :: BAPMetricsContainer,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    kafkaProducerTools :: KafkaProducerTools,
    kafkaEnvs :: BAPKafkaEnvs,
    rideCfg :: RideConfig,
    dashboardToken :: Text,
    cacheConfig :: CacheConfig,
    cacheTranslationConfig :: CacheTranslationConfig
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- getPodName
  isShuttingDown <- newEmptyTMVarIO
  bapMetrics <- registerBAPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  kafkaEnvs <- buildBAPKafkaEnvs
  hedisEnv <- connectHedis hedisCfg riderAppPrefix
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

data BAPs a = BAPs
  { metro :: a,
    cabs :: a
  }
  deriving (Generic, FromDhall)

type HasBapInfo r m =
  ( HasField "bapSelfIds" r (BAPs Text),
    HasField "bapSelfURIs" r (BAPs BaseUrl),
    MonadReader r m
  )

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.signingKey)
  getSignatureExpiry = (.signatureExpiry)

instance Registry Flow where
  registryLookup registryUrl =
    Registry.withSubscriberCache $
      Registry.whitelisting isWhiteListed <=< Registry.registryLookup registryUrl
    where
      isWhiteListed subscriberId = findByShortId (ShortId subscriberId) <&> isNothing

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . ("taxi-bap:registry:" <>) . lookupRequestToRedisKey
  setKey = Redis.set . ("taxi-bap:registry:" <>) . lookupRequestToRedisKey
  delKey = Redis.del . ("taxi-bap:registry:" <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . ("taxi-bap:registry:" <>) . lookupRequestToRedisKey

data RideConfig = RideConfig
  { driverReachedDistance :: Meters,
    driverOnTheWayNotifyExpiry :: Seconds
  }
  deriving (Generic, FromDhall)
