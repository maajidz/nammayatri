{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration.DecimalValue as M.DecimalValue
import qualified Beckn.Types.Core.Migration.Order as M.Order
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Callback (WithBecknCallback, WithBecknCallbackMig, withBecknCallback, withBecknCallbackMig)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Combinators hiding (Context)
import qualified Data.List as List
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (drop)
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
import qualified Types.Beckn.FmdItem as Item
import Types.Beckn.FmdOrder
import Types.Common
import Types.Error
import Types.Wrapper
import Utils.Common

search :: Org.Organization -> API.BecknReq SearchAPI.SearchIntent -> Flow AckResponse
search org req = do
  config@DunzoConfig {..} <- dzConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUriMig (req.context) dzBPNwAddress
  bap <- Org.findByBapUrl context.bap_uri >>= fromMaybeM OrgDoesNotExist
  dzBACreds <- getDzBAPCreds bap
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  withCallbackMig "search" SearchAPI.onSearchAPI context cbUrl $
    getQuote dzBACreds config quoteReq
      <&> mkOnSearchCatalog

select :: Org.Organization -> API.BecknReq SelectAPI.SelectedObject -> Flow AckResponse
select _org _req = throwError $ ActionNotSupported "select"

init :: Org.Organization -> API.BecknReq InitAPI.InitOrder -> Flow AckResponse
init org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUriMig (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  -- validateReturn order FIXME
  dzBACreds <- getDzBAPCreds org
  withCallbackMig "init" InitAPI.onInitAPI context cbUrl $ do
    let order = req.message
    onInitMessage <-
      mkQuoteReqFromInitOrder order
        >>= getQuote dzBACreds conf
        >>= mkOnInitMessage
          dzQuotationTTLinMin
          order
    createCaseIfNotPresent (getId $ org.id) onInitMessage
    return $ InitAPI.InitializedObject onInitMessage
  where
    createCaseIfNotPresent orgId onInitMessage = do
      now <- getCurrentTime
      let caseId = Id req.context.transaction_id
      let case_ =
            Case
              { id = caseId,
                name = Nothing,
                description = Nothing,
                shortId = "", -- FIX this
                industry = GROCERY,
                _type = RIDEORDER,
                exchangeType = ORDER,
                status = NEW,
                startTime = now,
                endTime = Nothing,
                validTill = now,
                provider = Just "Dunzo",
                providerType = Nothing,
                requestor = Just orgId,
                requestorType = Nothing,
                parentCaseId = Nothing,
                fromLocationId = "",
                toLocationId = "",
                udf1 = Just $ encodeToText onInitMessage,
                udf2 = Nothing,
                udf3 = Nothing,
                udf4 = Nothing,
                udf5 = Nothing,
                info = Nothing,
                createdAt = now,
                updatedAt = now
              }
      mcase <- Storage.findById caseId
      case mcase of
        Nothing -> Storage.create case_
        Just _ -> pass

confirm :: Org.Organization -> API.BecknReq API.OrderObject -> Flow AckResponse
confirm org req = do
  dconf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUriMig (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let reqOrder = req.message.order
  orderId <- fromMaybeErr "INVALID_ORDER_ID" (Just CORE003) $ reqOrder.id
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  (orderDetails :: OrderDetailsMig) <- case_.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let order = orderDetails.order
  validateDelayFromInit dzQuotationTTLinMin case_
  verifyPayment reqOrder order
  -- validateReturn order FIXME
  txnId <-
    reqOrder ^? #payment . #params . _Just . #transaction_id
      & fromMaybeErr "TXN_ID_NOT_PRESENT" Nothing
  let updatedOrderDetailsWTxn =
        orderDetails & ((#order . #payment . #params . _Just . #transaction_id) .~ txnId)
  dzBACreds <- getDzBAPCreds org
  withCallbackMig "confirm" ConfirmAPI.onConfirmAPI context cbUrl $ do
    taskStatus <- createTaskAPI dzBACreds dconf =<< mkCreateTaskReq order
    currTime <- getCurrentTime
    let uOrder = updateOrderMig currTime (updatedOrderDetailsWTxn.order) taskStatus
    checkAndLogPriceDiff (updatedOrderDetailsWTxn.order) uOrder
    updateCase case_ (updatedOrderDetailsWTxn & #order .~ uOrder) taskStatus
    return $ API.OrderObject uOrder
  where
    verifyPayment :: M.Order.Order -> M.Order.Order -> Flow ()
    verifyPayment reqOrder order = do
      confirmAmount <-
        reqOrder ^? #payment . #params . _Just . #amount
          & fromMaybeErr "INVALID_PAYMENT_AMOUNT" (Just CORE003)
      orderAmount <-
        order ^? #payment . #params . _Just . #amount
          & fromMaybeErr "ORDER_AMOUNT_NOT_FOUND" (Just CORE003)
      if confirmAmount == orderAmount
        then pass
        else throwError (InvalidRequest "Invalid order amount.")

    updateCase case_ orderDetails taskStatus = do
      let caseId = case_.id
      let taskId = taskStatus.task_id
      let updatedCase =
            case_
              { shortId = ShortId $ getTaskId taskId,
                udf1 = Just $ encodeToText orderDetails,
                udf2 = Just $ encodeToText taskStatus
              }
      Storage.update caseId updatedCase

    createTaskAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      API.createTask dzClientId token dzUrl dzTestMode req'

    checkAndLogPriceDiff initOrder confirmOrder = do
      let orderId = fromMaybe "" $ initOrder.id
      let initPrice = M.DecimalValue.convertDecimalValueToAmount =<< (initOrder.payment ^? #params . _Just . #amount . _Just)
      let confirmPrice = M.DecimalValue.convertDecimalValueToAmount =<< (confirmOrder.payment ^? #params . _Just . #amount . _Just)
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            logTagInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

    validateDelayFromInit dzQuotationTTLinMin case_ = do
      now <- getCurrentTime
      let orderCreatedAt = case_.createdAt
      let thresholdTime = addUTCTime (fromInteger (dzQuotationTTLinMin * 60)) orderCreatedAt
      when (thresholdTime < now) $
        throwError (InvalidRequest "Took too long to confirm.")

track :: Org.Organization -> API.BecknReq TrackAPI.TrackInfo -> Flow AckResponse
track org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let orderId = req.message.order_id
  let context = updateBppUriMig (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  withCallbackMig "track" TrackAPI.onTrackAPI context cbUrl $ do
    let taskId = getShortId case_.shortId
    dzBACreds <- getDzBAPCreds org
    mbTrackingUrl <- getStatus dzBACreds conf (TaskId taskId) >>= (maybe (pure Nothing) ((Just <$>) . parseBaseUrl) . (.tracking_url))
    return $ mkOnTrackMessage mbTrackingUrl

status :: Org.Organization -> API.BecknReq StatusAPI.OrderId -> Flow AckResponse
status org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUriMig (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let orderId = req.message.order_id
  case_ <- Storage.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = getShortId case_.shortId
  (orderDetails :: OrderDetailsMig) <-
    case_.udf1 >>= decodeFromText
      & fromMaybeM (InternalError "Decode error.")
  dzBACreds <- getDzBAPCreds org
  withCallbackMig "status" StatusAPI.onStatusAPI context cbUrl $ do
    taskStatus <- getStatus dzBACreds conf (TaskId taskId)
    let order = orderDetails.order
    onStatusMessage <- mkOnStatusMessage order taskStatus
    let updatedOrder = onStatusMessage.order
    let updatedOrderDetails = orderDetails & #order .~ updatedOrder
    updateCase (case_.id) updatedOrderDetails taskStatus case_
    return onStatusMessage
  where
    updateCase caseId orderDetails taskStatus case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText orderDetails, udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

cancel :: Org.Organization -> API.BecknReq CancelAPI.CancellationInfo -> Flow AckResponse
cancel org req = do
  let oId = req.message.order_id
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUriMig (req.context) dzBPNwAddress
  cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  case_ <- Storage.findById (Id oId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  let taskId = getShortId $ case_.shortId
  orderDetails <- case_.udf1 >>= decodeFromText & fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  dzBACreds <- getDzBAPCreds org
  withCallbackMig "cancel" CancelAPI.onCancelAPI context cbUrl $ do
    callCancelAPI dzBACreds conf (TaskId taskId)
    let updatedOrder = cancelOrder (orderDetails.order)
    let updatedOrderDetails = orderDetails & #order .~ updatedOrder
    updateCase case_.id updatedOrderDetails case_
    return $ API.OrderObject updatedOrder
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token dzUrl dzTestMode taskId ""

    updateCase :: Id Case -> OrderDetailsMig -> Case -> Flow ()
    updateCase caseId orderDetails case_ = do
      let updatedCase = case_ {udf1 = Just $ encodeToText orderDetails}
      Storage.update caseId updatedCase

update :: Org.Organization -> API.BecknReq UpdateAPI.UpdateInfo -> Flow AckResponse
update _org _req = throwError $ ActionNotSupported "update"

-- Helpers
getQuote :: DzBAConfig -> DunzoConfig -> QuoteReq -> Flow QuoteRes
getQuote ba@DzBAConfig {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  API.getQuote dzClientId token dzUrl quoteReq

getStatus :: DzBAConfig -> DunzoConfig -> TaskId -> Flow TaskStatus
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  API.taskStatus dzClientId token dzUrl dzTestMode taskId

fetchToken :: DzBAConfig -> DunzoConfig -> Flow Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- Dz.getToken dzClientId
  case mToken of
    Nothing -> do
      TokenRes token <- API.getToken dzTokenUrl (TokenReq dzClientId dzClientSecret)
      Dz.insertToken dzClientId token
      return token
    Just token -> return token

validateReturn :: Order -> Flow ()
validateReturn currOrder =
  when (currOrder._type == Just "RETURN") $ do
    prevOrderId <- currOrder.prev_order_id & fromMaybeM (InvalidRequest "Prev order id is null.")
    prevOrderCase <- Storage.findById (Id prevOrderId) >>= fromMaybeM CaseDoesNotExist
    (prevOrderDetails :: OrderDetails) <-
      prevOrderCase.udf1 >>= decodeFromText
        & fromMaybeM (InvalidRequest "Decode error.")
    let prevOrder = prevOrderDetails.order
    -- validating that the items which are returned should be a subset of items in the actual order.
    -- would fail when there are duplicates in current order items
    unless (null $ (Item.id <$> currOrder.items) List.\\ (Item.id <$> prevOrder.items)) $
      throwError (InvalidRequest "Invalid return order.")

withCallback :: WithBecknCallback api callback_success r
withCallback = withBecknCallback (Just HttpSig.signatureAuthManagerKey)

withCallbackMig :: WithBecknCallbackMig api callback_success r
withCallbackMig = withBecknCallbackMig (Just HttpSig.signatureAuthManagerKey)
