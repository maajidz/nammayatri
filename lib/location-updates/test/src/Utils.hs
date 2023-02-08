module Utils where

import qualified Data.Map as Map
import qualified EulerHS.Runtime as R
import Kernel.External.Encryption (EncTools (..))
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import qualified Kernel.Storage.Hedis.Queries as Hedis
import qualified Kernel.Tools.Metrics.CoreMetrics.Types as Metrics
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.SignatureAuth
import qualified "mock-google" Lib.IntegrationTests.Environment as Environment
import Network.HTTP.Client

data Person

-------------------------------------------------
--------------------run types--------------------
data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    hedisEnv :: HedisEnv,
    encTools :: EncTools,
    coreMetrics :: Metrics.CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions
  }
  deriving (Generic)

type TestM = FlowR AppEnv

runFlow :: Text -> AppEnv -> FlowR AppEnv a -> IO a
runFlow tag appEnv flow = do
  liftIO $
    R.withFlowRuntime Nothing $ \flowRt -> do
      flowRt' <-
        runFlowR flowRt appEnv $
          addAuthManagersToFlowRt flowRt [(Just defaultHttpClientOptions.timeoutMs, Map.singleton "default" defaultManagerSettings)]
      -- FIXME: this is a termporary solution, better fix core code relating to these managers
      runFlowR flowRt' appEnv $ withLogTag tag flow

defaultHttpClientOptions :: HttpClientOptions
defaultHttpClientOptions =
  HttpClientOptions
    { timeoutMs = 2000
    }

wrapTests :: (Environment.AppCfg -> AppEnv -> IO a) -> IO a
wrapTests func = do
  withHedisEnv defaultHedisCfg ("locationUpdatesTest:" <>) $ \hedisEnv -> do
    let loggerConfig = defaultLoggerConfig {logToFile = True, prettyPrinting = True}
    withLoggerEnv loggerConfig Nothing $ \loggerEnv -> do
      coreMetrics <- Metrics.registerCoreMetricsContainer
      -- fetch google configs for using mock-google or real google
      appCfg <- Environment.readConfig "../"
      let appEnv =
            AppEnv
              { httpClientOptions = defaultHttpClientOptions,
                encTools = appCfg.encTools,
                ..
              }
      func appCfg appEnv

------------------- utility functions ---------------------

incrDistance :: Id Person -> Double -> TestM Double
incrDistance driverId = Hedis.incrByFloat driverId.getId

updateDistanceTest :: Id Person -> HighPrecMeters -> TestM ()
updateDistanceTest driverId dist = void $ incrDistance driverId (realToFrac dist)

checkTraveledDistance :: Id Person -> TestM Double
checkTraveledDistance driverId = incrDistance driverId 0

deleteDistanceKey :: Id Person -> TestM ()
deleteDistanceKey driverId = Hedis.del driverId.getId

equalsEps :: Double -> Double -> Double -> Bool
equalsEps eps x y = abs (x - y) < eps

----------------- fixtures ---------------------------------

osrmConfig :: MapsServiceConfig
osrmConfig =
  OSRMConfig
    OSRMCfg
      { osrmUrl = fromJust $ parseBaseUrl "localhost:5000",
        radiusDeviation = Just 20
      }
