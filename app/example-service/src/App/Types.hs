module App.Types where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Shutdown

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

type MobileNumber = Text

data AppEnv = AppEnv
  { config :: AppCfg,
    isShuttingDown :: Shutdown
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
