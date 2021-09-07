{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Flow where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Combinators hiding (Context)
import EulerHS.Prelude hiding (drop)
import qualified EulerHS.Types as ET
import qualified ExternalAPI.Dunzo.Flow as API
import ExternalAPI.Dunzo.Types
import Product.Dunzo.Transform
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.Organization as Org
import qualified Types.Beckn.API.Cancel as CancelAPI
import qualified Types.Beckn.API.Confirm as ConfirmAPI
import qualified Types.Beckn.API.Init as InitAPI
import qualified Types.Beckn.API.Search as SearchAPI
import qualified Types.Beckn.API.Select as SelectAPI
import qualified Types.Beckn.API.Status as StatusAPI
import qualified Types.Beckn.API.Track as TrackAPI
import qualified Types.Beckn.API.Types as API
import qualified Types.Beckn.API.Update as UpdateAPI
import Types.Beckn.Context (Action (..))
import Types.Beckn.DecimalValue (convertDecimalValueToAmount)
import Types.Beckn.Order (Order)
import Types.Common
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.Case as Case
import qualified Types.Storage.Organization as Org
import Types.Wrapper
import Utils.Common

search ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    HasHttpClientOptions r,
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq SearchAPI.SearchIntent ->
  m AckResponse
search org req = do
  config@DunzoConfig {..} <- asks (.dzConfig)
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req.context) dzBPNwAddress
  bap <- Org.findByBapUrl context.bap_uri >>= fromMaybeM OrgDoesNotExist
  dzBACreds <- getDzBAPCreds bap
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  withBecknCallbackMig withRetry authKey SEARCH SearchAPI.onSearchAPI context cbUrl $
    getQuote dzBACreds config quoteReq
      <&> mkOnSearchCatalog

select :: MonadFlow m => Org.Organization -> API.BecknReq SelectAPI.SelectedObject -> m AckResponse
select _org _req = throwError $ ActionNotSupported "select"

init :: MonadFlow m => Org.Organization -> API.BecknReq InitAPI.InitOrderObj -> m AckResponse
init _org _req = throwError $ ActionNotSupported "init"

confirm ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq API.OrderObject ->
  m AckResponse
confirm org req = do
  dconf@DunzoConfig {..} <- asks (.dzConfig)
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let reqOrder = req.message.order
  whenJust reqOrder.id \_ -> throwError $ InvalidRequest "Order id must not be presented."
  whenNothing_
    (reqOrder ^. #payment . #params . _Just . #transaction_id)
    $ throwError (ErrorCodeWithMessage "TXN_ID_NOT_PRESENT" CORE001)
  dzBACreds <- getDzBAPCreds org
  orderId <- generateGUID
  currTime <- getCurrentTime
  taskReq <- mkCreateTaskReq orderId reqOrder
  withCallback CONFIRM ConfirmAPI.onConfirmAPI context cbUrl $ do
    taskStatus <- createTaskAPI dzBACreds dconf taskReq
    let uOrder = updateOrder (Just orderId) currTime reqOrder taskStatus
    checkAndLogPriceDiff reqOrder uOrder
    createCase orderId uOrder taskStatus currTime
    return $ API.OrderObject uOrder
  where
    createCase orderId order taskStatus now = do
      let caseId = Id req.context.transaction_id
      let case_ =
            Case
              { id = caseId,
                name = Nothing,
                description = Nothing,
                shortId = ShortId orderId,
                industry = GROCERY,
                _type = RIDEORDER,
                exchangeType = ORDER,
                status = CONFIRMED,
                startTime = now,
                endTime = Nothing,
                validTill = now,
                provider = Just "Dunzo",
                providerType = Nothing,
                requestor = Just org.id.getId,
                requestorType = Nothing,
                parentCaseId = Nothing,
                udf1 = Just $ encodeToText order,
                udf2 = Just $ encodeToText taskStatus,
                udf3 = Nothing,
                udf4 = Nothing,
                udf5 = Nothing,
                info = Nothing,
                createdAt = now,
                updatedAt = now
              }
      Storage.create case_

    createTaskAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      API.createTask dzClientId token dzUrl dzTestMode req'

    checkAndLogPriceDiff initOrder confirmOrder = do
      let orderId = fromMaybe "" $ initOrder.id
      let initPrice = convertDecimalValueToAmount =<< (initOrder.payment ^? #params . _Just . #amount . _Just)
      let confirmPrice = convertDecimalValueToAmount =<< (confirmOrder.payment ^? #params . _Just . #amount . _Just)
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            logTagInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

track ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq TrackAPI.TrackInfo ->
  m AckResponse
track org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let orderId = req.context.transaction_id
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: TaskStatus <- case_.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  dzBACreds <- getDzBAPCreds org
  withCallback TRACK TrackAPI.onTrackAPI context cbUrl $
    getStatus dzBACreds conf (TaskId taskId) <&> mkOnTrackMessage . (.tracking_url)

status ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq StatusAPI.OrderId ->
  m AckResponse
status org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let orderId = context.transaction_id
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: TaskStatus <- case_.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  order <-
    case_.udf1 >>= decodeFromText
      & fromMaybeM (InternalError "Decode error.")
  dzBACreds <- getDzBAPCreds org
  withCallback STATUS StatusAPI.onStatusAPI context cbUrl $ do
    taskStatus <- getStatus dzBACreds conf (TaskId taskId)
    onStatusMessage <- mkOnStatusMessage order taskStatus
    let updatedOrder = onStatusMessage.order
    updateCase (case_.id) updatedOrder taskStatus case_
    return onStatusMessage
  where
    updateCase caseId order taskStatus case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText order, udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

cancel ::
  ( DBFlow m r,
    HasFlowEnv m r '["dzConfig" ::: DunzoConfig],
    CoreMetrics m
  ) =>
  Org.Organization ->
  API.BecknReq CancelAPI.CancellationInfo ->
  m AckResponse
cancel org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let context = updateBppUri (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id context.transaction_id) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: TaskStatus <- case_.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  order <- case_.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  dzBACreds <- getDzBAPCreds org
  withCallback CANCEL CancelAPI.onCancelAPI context cbUrl $ do
    callCancelAPI dzBACreds conf (TaskId taskId)
    let updatedOrder = cancelOrder order
    updateCase case_.id updatedOrder case_
    return $ API.OrderObject updatedOrder
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token dzUrl dzTestMode taskId ""

    updateCase :: DBFlow m r => Id Case -> Order -> Case -> m ()
    updateCase caseId order case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText order}
      Storage.update caseId updatedCase

update :: MonadFlow m => Org.Organization -> API.BecknReq UpdateAPI.UpdateInfo -> m AckResponse
update _org _req = throwError $ ActionNotSupported "update"

-- Helpers
getQuote ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  DzBAConfig ->
  DunzoConfig ->
  QuoteReq ->
  m QuoteRes
getQuote ba@DzBAConfig {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  API.getQuote dzClientId token dzUrl quoteReq

getStatus ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  DzBAConfig ->
  DunzoConfig ->
  TaskId ->
  m TaskStatus
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  API.taskStatus dzClientId token dzUrl dzTestMode taskId

fetchToken ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  DzBAConfig ->
  DunzoConfig ->
  m Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- Dz.getToken dzClientId
  case mToken of
    Nothing -> do
      TokenRes token <- API.getToken dzTokenUrl (TokenReq dzClientId dzClientSecret)
      Dz.insertToken dzClientId token
      return token
    Just token -> return token

withCallback :: WithBecknCallbackMig api callback_success m
withCallback = withBecknCallbackMig identity authKey

authKey :: Maybe ET.ManagerSelector
authKey = Just HttpSig.signatureAuthManagerKey
