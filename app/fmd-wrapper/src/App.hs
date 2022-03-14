module App
  ( runFMDWrapper,
  )
where

import App.Server
import App.Types
import Beckn.Exit
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Tools.Metrics.Init as Metrics
import qualified Beckn.Types.App as App
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Beckn.Utils.FlowLogging as L
import Beckn.Utils.Migration
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import System.Environment (lookupEnv)
import Utils.Common

runFMDWrapper :: (AppCfg -> AppCfg) -> IO ()
runFMDWrapper configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "fmd-wrapper"
  Metrics.serve (appCfg.metricsPort)
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  appEnv <- buildAppEnv appCfg
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
          & setPort (appCfg.port)
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        managers <-
          prepareAuthManager flowRt appEnv ["Authorization"] appCfg.selfId appCfg.authEntity.uniqueKeyId
            & Map.singleton signatureAuthManagerKey
            & createManagers
        try (prepareRedisConnections $ appCfg.redisCfg)
          >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
        migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.dbCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo ("Runtime created. Starting server at port " <> show (appCfg.port))
        return $ flowRt {R._httpClientManagers = managers}
    runSettings settings $ run $ App.EnvR flowRt' appEnv
