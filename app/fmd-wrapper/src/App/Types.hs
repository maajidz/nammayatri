module App.Types
  ( AppCfg (),
    AppEnv (..),
    Env,
    Flow,
    FlowHandler,
    FlowServer,
    mkAppEnv,
  )
where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Monitoring.Prometheus.Metrics
import Beckn.Utils.Servant.SignatureAuth
import Data.Time
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Prometheus as P
import Types.Wrapper (DelhiveryConfig, DunzoConfig)

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    selfId :: Text,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    coreVersion :: Text,
    domainVersion :: Text,
    dzConfig :: DunzoConfig,
    dlConfig :: DelhiveryConfig,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: NominalDiffTime
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    selfId :: Text,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    coreVersion :: Text,
    domainVersion :: Text,
    dzConfig :: DunzoConfig,
    dlConfig :: DelhiveryConfig,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: NominalDiffTime,
    metricsRequestLatencyHistogram :: P.Vector P.Label3 P.Histogram
  }
  deriving (Generic)

mkAppEnv :: AppCfg -> IO AppEnv
mkAppEnv AppCfg {..} = do
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
  getSelfId = selfId
  getSelfUrl = xGatewayUri
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry

instance HasCoreMetrics Flow where
  getRequestLatencyHistogram = metricsRequestLatencyHistogram <$> ask
