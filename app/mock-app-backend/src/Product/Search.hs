{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( searchCb,
    triggerSearch,
  )
where

import App.Types
import App.Utils
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.FMD.API.Search
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (BaseUrl (..), Scheme (..))
import qualified System.Environment as SE

gatewayLookup :: FlowR r (String, Int)
gatewayLookup =
  EL.runIO $
    (,)
      <$> (fromMaybe "localhost" <$> SE.lookupEnv "GATEWAY_HOST")
      <*> (fromMaybe 8015 . (>>= readMaybe) <$> SE.lookupEnv "GATEWAY_PORT")

gatewayBaseUrl :: FlowR r BaseUrl
gatewayBaseUrl = do
  (host, port) <- gatewayLookup
  return
    BaseUrl
      { baseUrlScheme = Http,
        baseUrlHost = host,
        baseUrlPort = port,
        baseUrlPath = "v1"
      }

triggerSearch :: FlowHandlerR r AckResponse
triggerSearch = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  baseUrl <- gatewayBaseUrl
  transactionId <- EL.generateGUID
  req <- EL.runIO $ buildSearchReq transactionId
  eRes <-
    callClient "search" baseUrl $
      client searchAPI "test-app-2-key" req
  EL.logDebug @Text "mock_app_backend" $ "context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = req ^. #context,
        _message = ack "ACK",
        _error = Nothing
      }

searchCb :: () -> OnSearchReq -> FlowHandler AckResponse
searchCb _unit req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  EL.logDebug @Text "mock_app_backend" $ "search_cb: req: " <> show (toJSON req) <> ", resp: " <> show resp
  return resp
