{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App (startKafkaConsumer, runDriverHealthcheck) where

import qualified Consumer.Flow as CF
import Control.Concurrent
import Data.Function
import DriverTrackingHealthCheck.API
import qualified DriverTrackingHealthCheck.Service.Runner as Service
import Environment
import qualified EulerHS.Runtime as L
import qualified EulerHS.Runtime as R
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Flow
import Kernel.Utils.Common hiding (id)
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import qualified Kernel.Utils.Servant.Server as Server
import Kernel.Utils.Shutdown
import Network.Wai.Handler.Warp
import Servant
import System.Environment (lookupEnv)

startKafkaConsumer :: IO ()
startKafkaConsumer = do
  consumerType :: ConsumerType <- read . fromMaybe "AVAILABILITY_TIME" <$> lookupEnv "CONSUMER_TYPE"
  configFile <- CF.getConfigNameFromConsumertype consumerType
  appCfg :: AppCfg <- readDhallConfigDefault configFile
  appEnv <- buildAppEnv appCfg consumerType
  when (consumerType == LOCATION_UPDATE) (void $ forkIO $ runDriverHealthcheck appCfg appEnv)
  runKafkaConsumer appCfg appEnv

runKafkaConsumer :: AppCfg -> AppEnv -> IO ()
runKafkaConsumer appCfg appEnv = do
  flowRt' <- L.createFlowRuntime' (Just $ L.getEulerLoggerRuntime appEnv.hostname appEnv.loggerConfig)
  managers <- managersFromManagersSettings appCfg.httpClientOptions.timeoutMs mempty -- default manager is created
  let flowRt = flowRt' {L._httpClientManagers = managers}
  startConsumerWithEnv flowRt appEnv

startConsumerWithEnv :: L.FlowRuntime -> AppEnv -> IO ()
startConsumerWithEnv flowRt appEnv@AppEnv {..} = do
  kafkaConsumer <- newKafkaConsumer
  CF.runConsumer flowRt appEnv consumerType kafkaConsumer
  where
    newKafkaConsumer =
      either (error . ("Unable to open a kafka consumer: " <>) . show) id
        <$> Consumer.newConsumer
          (kafkaConsumerCfg.consumerProperties)
          (Consumer.topics kafkaConsumerCfg.topicNames)

runDriverHealthcheck :: AppCfg -> AppEnv -> IO ()
runDriverHealthcheck appCfg appEnv = do
  Metrics.serve appCfg.metricsPort
  let heathCheckConfig = fromJust appCfg.healthCheckAppCfg
  let loggerRt = L.getEulerLoggerRuntime appEnv.hostname heathCheckConfig.loggerConfig

  R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv do
      managers <- createManagers mempty -- default manager is created
      pure $ flowRt {R._httpClientManagers = managers}

    let settings =
          defaultSettings
            & setGracefulShutdownTimeout (Just $ getSeconds heathCheckConfig.graceTerminationPeriod)
            & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
            & setPort heathCheckConfig.healthcheckPort
    void . forkIO . runSettings settings $ Server.run healthCheckAPI healthCheck EmptyContext (App.EnvR flowRt' appEnv)

    runFlowR flowRt' appEnv $ Service.driverTrackingHealthcheckService heathCheckConfig
    waitForShutdown appEnv.isShuttingDown
