{-# LANGUAGE OverloadedLabels #-}

module MockAppBackend.OnSearch where

import Data.Time
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import MockAppBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec

spec :: Spec
spec = do
  mockAppManager <- runIO $ Client.newManager tlsManagerSettings
  let appClientEnv = mkClientEnv mockAppManager mockAppBaseUrl
      loggerCfg = getLoggerConfig "mock-app-backend"
  around (withFlowRuntime (Just loggerCfg)) $
    describe "Mock App Backend OnSearch Api" $
      it "should return valid ack response" $
        \_flowRt ->
          do
            now <- getCurrentTime
            let ctx = buildContext "on_search" "dummy-txn-id" now
                onSearchReq = buildOnSearchReq ctx
            eitherSearchCbRes <- runClient appClientEnv $ onSearchFlow "" onSearchReq
            eitherSearchCbRes `shouldSatisfy` isRight

            return ()
