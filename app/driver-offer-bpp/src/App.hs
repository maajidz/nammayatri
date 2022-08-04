module App where

import qualified App.Server as App
import Beckn.Exit
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded)
import Beckn.Storage.Redis.Config
import qualified Beckn.Tools.Metrics.Init as Metrics
import qualified Beckn.Types.App as App
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Dhall
import qualified Beckn.Utils.FlowLogging as L
import qualified Data.Text as T
import Environment
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import qualified Storage.Queries.Organization as Storage
import System.Environment (lookupEnv)
import Tools.SignatureAuth
import Utils.Common

runDriverOfferBpp :: (AppCfg -> AppCfg) -> IO ()
runDriverOfferBpp configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "driver-offer-bpp"
  Metrics.serve (appCfg.metricsPort)
  runDriverOfferBpp' appCfg

runDriverOfferBpp' :: AppCfg -> IO ()
runDriverOfferBpp' appCfg = do
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
          & setPort (appCfg.port)
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "

        logInfo "Setting up for signature auth..."
        allProviders <-
          try Storage.loadAllProviders
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allShortIds = map ((.shortId.getShortId) &&& (.uniqueKeyId)) allProviders
        flowRt' <- modFlowRtWithAuthManagersWithRegistryUrl flowRt appEnv allShortIds

        logInfo "Initializing Redis Connections..."
        try (prepareRedisConnections $ appCfg.redisCfg)
          >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "

        logInfo ("Runtime created. Starting server at port " <> show (appCfg.port))
        pure flowRt'
    runSettings settings $ App.run (App.EnvR flowRt' appEnv)
