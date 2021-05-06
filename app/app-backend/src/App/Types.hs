module App.Types
  ( Env,
    Flow,
    FlowHandler,
    FlowServer,
    AppCfg (),
    AppEnv (..),
    mkAppEnv,
  )
where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.SesConfig (SesConfig)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Monitoring.Prometheus.Metrics
import Beckn.Utils.Servant.SignatureAuth
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Prometheus as P
import Types.Geofencing
import Utils.Metrics

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    sesCfg :: SesConfig,
    port :: Int,
    metricsPort :: Int,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    xProviderUri :: BaseUrl,
    bapSelfId :: Text,
    bapNwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    searchConfirmExpiry :: Maybe Integer,
    searchCaseExpiry :: Maybe Integer,
    cronAuthKey :: Maybe CronAuthKey,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    geofencingConfig :: GeofencingConfig,
    traceFlag :: TraceFlag,
    signatureExpiry :: NominalDiffTime,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    sesCfg :: SesConfig,
    xGatewayUri :: BaseUrl,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    xProviderUri :: BaseUrl,
    bapSelfId :: Text,
    bapNwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    searchConfirmExpiry :: Maybe Integer,
    searchCaseExpiry :: Maybe Integer,
    cronAuthKey :: Maybe CronAuthKey,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    domainVersion :: Text,
    geofencingConfig :: GeofencingConfig,
    traceFlag :: TraceFlag,
    signatureExpiry :: NominalDiffTime,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    metricsCaseCounter :: P.Vector P.Label2 P.Counter,
    metricsSearchDurationHistogram :: P.Histogram,
    metricsRequestLatencyHistogram :: P.Vector P.Label3 P.Histogram
  }
  deriving (Generic)

mkAppEnv :: AppCfg -> IO AppEnv
mkAppEnv AppCfg {..} = do
  metricsCaseCounter <- registerCaseCounter
  metricsSearchDurationHistogram <- registerSearchDurationHistogram
  metricsRequestLatencyHistogram <- registerRequestLatencyHistogram
  return $
    AppEnv
      { ..
      }

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSelfId = bapSelfId
  getSelfUrl = bapNwAddress
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry

instance HasBAPMetrics Flow where
  getCaseCounter = metricsCaseCounter <$> ask
  getSearchDurationHistogram = metricsSearchDurationHistogram <$> ask

instance HasCoreMetrics Flow where
  getRequestLatencyHistogram = metricsRequestLatencyHistogram <$> ask
