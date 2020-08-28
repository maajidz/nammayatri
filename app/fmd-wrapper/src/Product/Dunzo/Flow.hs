{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{-# HLINT ignore "Reduce duplication" #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.App (CaseId (..), _getOrganizationId)
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue (convertDecimalValueToAmount)
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes, onCancelAPI)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes, onConfirmAPI)
import Beckn.Types.FMD.API.Init (InitReq, InitRes, onInitAPI)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes, onSearchAPI)
import Beckn.Types.FMD.API.Select (DraftOrder (..), SelectReq (..), SelectRes, onSelectAPI)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes, onStatusAPI)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes, onTrackAPI)
import Beckn.Types.FMD.API.Update (UpdateReq, UpdateRes, onUpdateAPI)
import qualified Beckn.Types.FMD.Item as Item
import Beckn.Types.FMD.Order
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (decodeFromText, encodeToText, fork, fromMaybeM400, fromMaybeM500, getCurrTime, throwJsonError400, throwJsonError500)
import Control.Lens ((?~))
import Control.Lens.Combinators hiding (Context)
import Data.Aeson
import qualified Data.List as List
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Dunzo.Flow as API
import External.Dunzo.Types
import Product.Dunzo.Transform
import Servant.Client (ClientError (..), ResponseF (..))
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Quote as Storage
import Types.Error
import Types.Wrapper
import Utils.Common (fromMaybe400Log)

search :: Org.Organization -> SearchReq -> Flow SearchRes
search org req = do
  config@DunzoConfig {..} <- dzConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  bapUrl <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_URI"
  bap <- Org.findByBapUrl bapUrl >>= fromMaybeM400 "BAP_NOT_CONFIGURED"
  dzBACreds <- getDzBAPCreds bap
  fork "Search" $ do
    eres <- getQuote dzBACreds config quoteReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "QuoteRes") $ show eres
    sendCb context eres
  returnAck context
  where
    sendCb context res = do
      cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
      cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
      case res of
        Right quoteRes -> do
          onSearchReq <- mkOnSearchReq org context quoteRes
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_search") $ "on_search cb req" <> show onSearchReq
          onSearchResp <- L.callAPI cbUrl $ ET.client onSearchAPI cbApiKey onSearchReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_search") $ "on_search cb resp" <> show onSearchResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSearchErrReq = mkOnSearchErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_search") $ "on_search cb err req" <> show onSearchErrReq
              onSearchResp <- L.callAPI cbUrl $ ET.client onSearchAPI cbApiKey onSearchErrReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_search") $ "on_search cb err resp" <> show onSearchResp
        _ -> pass

select :: Org.Organization -> SelectReq -> Flow SelectRes
select org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let ctx = updateBppUri (req ^. #context) dzBPNwAddress
  validateReturn $ req ^. #message . #order
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  dzBACreds <- getDzBAPCreds org
  fork "Select" do
    quoteReq <- mkQuoteReqFromSelect req
    eres <- getQuote dzBACreds conf quoteReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "QuoteRes") $ show eres
    sendCallback ctx cbUrl cbApiKey eres
  returnAck ctx
  where
    sendCallback context cbUrl cbApiKey res =
      case res of
        Right quoteRes -> do
          let reqOrder = req ^. #message . #order
          onSelectMessage <- mkOnSelectMessage reqOrder quoteRes
          let onSelectReq = mkOnSelectReq context onSelectMessage
          let order = onSelectMessage ^. #order
          let quote = onSelectMessage ^. #quote
          let quoteId = quote ^. #_id
          let orderDetails = OrderDetails order quote
          Storage.storeQuote quoteId orderDetails
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_select") $ "on_select cb req" <> show onSelectReq
          onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_select") $ "on_select cb resp" <> show onSelectResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSelectReq = mkOnSelectErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_select") $ "on_select cb err req" <> show onSelectReq
              onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_select") $ "on_select cb err resp" <> show onSelectResp
        _ -> pass

init :: Org.Organization -> InitReq -> Flow InitRes
init org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  quote <- req ^. (#message . #order . #_quotation) & fromMaybe400Log "INVALID_QUOTATION" (Just CORE003) context
  let quoteId = quote ^. #_id
  paymentTerms <- paymentPolicy & decodeFromText & fromMaybeM500 "PAYMENT_POLICY_DECODE_ERROR"
  payeeDetails <- payee & decodeFromText & fromMaybeM500 "PAYMENT_ENDPOINT_DECODE_ERROR"
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybe400Log "INVALID_QUOTATION_ID" (Just CORE003) context
  let order = orderDetails ^. #order
  validateReturn order
  dzBACreds <- getDzBAPCreds org
  fork "init" do
    quoteReq <- mkQuoteReqFromSelect $ SelectReq context (DraftOrder (orderDetails ^. #order))
    eres <- getQuote dzBACreds conf quoteReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "QuoteRes") $ show eres
    sendCb orderDetails context cbApiKey cbUrl paymentTerms payeeDetails quoteId eres
  returnAck context
  where
    sendCb orderDetails context cbApiKey cbUrl paymentTerms payeeDetails quoteId (Right res) = do
      -- quoteId will be used as orderId
      onInitMessage <-
        mkOnInitMessage
          quoteId
          (orderDetails ^. #order)
          paymentTerms
          payeeDetails
          req
          res
      let onInitReq = mkOnInitReq context onInitMessage
      createCaseIfNotPresent (_getOrganizationId $ org ^. #_id) (onInitMessage ^. #order) (orderDetails ^. #quote)
      onInitResp <- L.callAPI cbUrl $ ET.client onInitAPI cbApiKey onInitReq
      L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_init") $ show onInitResp
      return ()
    sendCb _ context cbApiKey cbUrl _ _ _ (Left (FailureResponse _ (Response _ _ _ body))) = do
      case decode body of
        Just err -> do
          let onInitReq = mkOnInitErrReq context err
          onInitResp <- L.callAPI cbUrl $ ET.client onInitAPI cbApiKey onInitReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_init err") $ show onInitResp
          return ()
        Nothing -> return ()
    sendCb _ _ _ _ _ _ _ _ = return ()

    createCaseIfNotPresent orgId order quote = do
      now <- getCurrTime
      let caseId = CaseId $ fromJust $ order ^. #_id
      let case_ =
            Case
              { _id = caseId,
                _name = Nothing,
                _description = Nothing,
                _shortId = "", -- FIX this
                _industry = GROCERY,
                _type = RIDEORDER,
                _exchangeType = ORDER,
                _status = NEW,
                _startTime = now,
                _endTime = Nothing,
                _validTill = now,
                _provider = Just "Dunzo",
                _providerType = Nothing,
                _requestor = Just orgId,
                _requestorType = Nothing,
                _parentCaseId = Nothing,
                _fromLocationId = "",
                _toLocationId = "",
                _udf1 = Just $ encodeToText (OrderDetails order quote),
                _udf2 = Nothing,
                _udf3 = Nothing,
                _udf4 = Nothing,
                _udf5 = Nothing,
                _info = Nothing,
                _createdAt = now,
                _updatedAt = now
              }
      mcase <- Storage.findById caseId
      case mcase of
        Nothing -> Storage.create case_
        Just _ -> pass

confirm :: Org.Organization -> ConfirmReq -> Flow ConfirmRes
confirm org req = do
  dconf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let reqOrder = req ^. (#message . #order)
  orderId <- fromMaybe400Log "INVALID_ORDER_ID" (Just CORE003) context $ reqOrder ^. #_id
  case_ <- Storage.findById (CaseId orderId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  (orderDetails :: OrderDetails) <- case_ ^. #_udf1 >>= decodeFromText & fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  let order = orderDetails ^. #order
  verifyPayment reqOrder order
  validateReturn order
  paymentTerms <- paymentPolicy & decodeFromText & fromMaybeM500 "PAYMENT_POLICY_DECODE_ERROR"
  payeeDetails <- payee & decodeFromText & fromMaybeM500 "PAYMENT_ENDPOINT_DECODE_ERROR"
  txnId <-
    reqOrder ^? #_payment . _Just . #_transaction_id
      & fromMaybe400Log "TXN_ID_NOT_FOUND" Nothing context
  let updatedOrderDetailsWTxn =
        orderDetails & ((#order . #_payment . _Just . #_transaction_id) .~ txnId)
  dzBACreds <- getDzBAPCreds org
  fork "confirm" do
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "Confirm") "Started"
    createTaskReq <- mkCreateTaskReq context order
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "CreateTaskReq") (encodeToText createTaskReq)
    eres <- createTaskAPI dzBACreds dconf createTaskReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "CreateTaskRes") $ show eres
    sendCb case_ updatedOrderDetailsWTxn context cbApiKey cbUrl paymentTerms payeeDetails eres
  returnAck context
  where
    verifyPayment :: Order -> Order -> Flow ()
    verifyPayment reqOrder order = do
      let context = req ^. #context
      confirmAmount <-
        reqOrder ^? #_payment . _Just . #_amount . #_value
          & fromMaybe400Log "INVALID_PAYMENT_AMOUNT" (Just CORE003) context
      orderAmount <-
        order ^? #_payment . _Just . #_amount . #_value
          & fromMaybe400Log "ORDER_AMOUNT_NOT_FOUND" (Just CORE003) context
      if confirmAmount == orderAmount
        then pass
        else throwJsonError400 "AMOUNT_VALIDATION_ERR" "INVALID_ORDER_AMOUNT"

    updateCase case_ orderDetails taskStatus = do
      let caseId = case_ ^. #_id
      let taskId = taskStatus ^. #task_id
      let updatedCase =
            case_
              { _shortId = getTaskId taskId,
                _udf1 = Just $ encodeToText orderDetails,
                _udf2 = Just $ encodeToText taskStatus
              }
      Storage.update caseId updatedCase

    createTaskAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      API.createTask dzClientId token dzUrl req'

    sendCb case_ orderDetails context cbApiKey cbUrl paymentTerms payeeDetails res = do
      case res of
        Right taskStatus -> do
          currTime <- getCurrTime
          let uOrder = updateOrder (org ^. #_name) currTime (orderDetails ^. #order) paymentTerms payeeDetails taskStatus
          checkAndLogPriceDiff (orderDetails ^. #order) uOrder
          updateCase case_ (orderDetails & #order .~ uOrder) taskStatus
          onConfirmReq <- mkOnConfirmReq context uOrder
          eres <- L.callAPI cbUrl $ ET.client onConfirmAPI cbApiKey onConfirmReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_confirm") $ show eres
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onConfirmReq = mkOnConfirmErrReq context err
              onConfirmResp <- L.callAPI cbUrl $ ET.client onConfirmAPI cbApiKey onConfirmReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_confirm err") $ show onConfirmResp
        _ -> pass

    checkAndLogPriceDiff initOrder confirmOrder = do
      let orderId = fromMaybe "" $ initOrder ^. #_id
      let initPrice = convertDecimalValueToAmount . (^. #_amount . #_value) =<< initOrder ^. #_payment
      let confirmPrice = convertDecimalValueToAmount . (^. #_amount . #_value) =<< confirmOrder ^. #_payment
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            L.logInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

track :: Org.Organization -> TrackReq -> Flow TrackRes
track org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let orderId = req ^. (#message . #order_id)
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  case_ <- Storage.findById (CaseId orderId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  fork "track" do
    let taskId = case_ ^. #_shortId
    dzBACreds <- getDzBAPCreds org
    eStatusRes <- getStatus dzBACreds conf (TaskId taskId)
    L.logInfo @Text "StatusRes" $ show eStatusRes
    case eStatusRes of
      Left _ -> do
        let onTrackErrReq = mkOnTrackErrReq context "Failed to fetch tracking URL"
        eres <- L.callAPI cbUrl $ ET.client onTrackAPI cbApiKey onTrackErrReq
        L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_track") $ show eres
      Right statusRes -> do
        let onTrackReq = mkOnTrackReq context (statusRes ^. #tracking_url)
        eres <- L.callAPI cbUrl $ ET.client onTrackAPI cbApiKey onTrackReq
        L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_track") $ show eres
  returnAck context

status :: Org.Organization -> StatusReq -> Flow StatusRes
status org req = do
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  paymentTerms <- paymentPolicy & decodeFromText & fromMaybeM500 "PAYMENT_POLICY_DECODE_ERROR"
  payeeDetails <- payee & decodeFromText & fromMaybeM500 "PAYMENT_ENDPOINT_DECODE_ERROR"
  let orderId = req ^. (#message . #order_id)
  c <- Storage.findById (CaseId orderId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  let taskId = c ^. #_shortId
  (orderDetails :: OrderDetails) <- c ^. #_udf1 >>= decodeFromText & fromMaybeM500 "ORDER_NOT_FOUND"
  dzBACreds <- getDzBAPCreds org
  fork "status" do
    eres <- getStatus dzBACreds conf (TaskId taskId)
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "StatusRes") $ show eres
    sendCb c orderDetails context cbApiKey cbUrl paymentTerms payeeDetails eres
  returnAck context
  where
    updateCase caseId orderDetails taskStatus case_ = do
      let updatedCase = case_ {_udf1 = Just $ encodeToText orderDetails, _udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

    callCbAPI cbApiKey cbUrl = L.callAPI cbUrl . ET.client onStatusAPI cbApiKey

    sendCb case_ orderDetails context cbApiKey cbUrl paymentTerms payeeDetails res = do
      let order = orderDetails ^. #order
      case res of
        Right taskStatus -> do
          onStatusMessage <- mkOnStatusMessage (org ^. #_name) order paymentTerms payeeDetails taskStatus
          onStatusReq <- mkOnStatusReq context onStatusMessage
          let updatedOrder = onStatusMessage ^. #order
          let updatedOrderDetails = orderDetails & #order .~ updatedOrder
          updateCase (case_ ^. #_id) updatedOrderDetails taskStatus case_
          onStatusRes <- callCbAPI cbApiKey cbUrl onStatusReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_status") $ show onStatusRes
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onStatusReq = mkOnStatusErrReq context err
              onStatusResp <- callCbAPI cbApiKey cbUrl onStatusReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_status err") $ show onStatusResp
        _ -> pass

cancel :: Org.Organization -> CancelReq -> Flow CancelRes
cancel org req = do
  let oId = req ^. (#message . #order . #id)
  conf@DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  case_ <- Storage.findById (CaseId oId) >>= fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  let taskId = case_ ^. #_shortId
  orderDetails <- case_ ^. #_udf1 >>= decodeFromText & fromMaybe400Log "ORDER_NOT_FOUND" (Just CORE003) context
  dzBACreds <- getDzBAPCreds org
  fork "cancel" do
    eres <- callCancelAPI dzBACreds conf (TaskId taskId)
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "CancelRes") $ show eres
    sendCb case_ orderDetails context cbApiKey cbUrl eres
  returnAck context
  where
    callCancelAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token dzUrl taskId ""

    updateCase :: CaseId -> OrderDetails -> Case -> Flow ()
    updateCase caseId orderDetails case_ = do
      let updatedOrderDetails = orderDetails & (#order . #_state) ?~ "CANCELLED"
          updatedCase = case_ {_udf1 = Just $ encodeToText updatedOrderDetails}
      Storage.update caseId updatedCase

    sendCb case_ orderDetails context cbApiKey cbUrl res =
      case res of
        Right () -> do
          let order = orderDetails ^. #order
          onCancelReq <- mkOnCancelReq context order
          updateCase (case_ ^. #_id) orderDetails case_
          onCancelRes <- L.callAPI cbUrl $ ET.client onCancelAPI cbApiKey onCancelReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_cancel") $ show onCancelRes
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onCancelReq = mkOnCancelErrReq context err
              onCancelResp <- L.callAPI cbUrl $ ET.client onCancelAPI cbApiKey onCancelReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_cancel err") $ show onCancelResp
        _ -> pass

update :: Org.Organization -> UpdateReq -> Flow UpdateRes
update org req = do
  DunzoConfig {..} <- dzConfig <$> ask
  let context = updateBppUri (req ^. #context) dzBPNwAddress
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  fork "update" do
    -- TODO: Dunzo doesnt have update
    let onUpdateReq = mkOnUpdateErrReq context
    eres <- L.callAPI cbUrl $ ET.client onUpdateAPI cbApiKey onUpdateReq
    L.logInfo @Text (req ^. #context . #_transaction_id <> "_" <> "on_update") $ show eres
  returnAck context

-- Helpers
getQuote :: DzBAConfig -> DunzoConfig -> QuoteReq -> Flow (Either ClientError QuoteRes)
getQuote ba@DzBAConfig {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  API.getQuote dzClientId token dzUrl quoteReq

getStatus :: DzBAConfig -> DunzoConfig -> TaskId -> Flow (Either ClientError TaskStatus)
getStatus dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} taskId = do
  token <- fetchToken dzBACreds conf
  API.taskStatus dzClientId token dzUrl taskId

fetchToken :: DzBAConfig -> DunzoConfig -> Flow Token
fetchToken DzBAConfig {..} DunzoConfig {..} = do
  mToken <- Dz.getToken dzClientId
  case mToken of
    Nothing -> do
      eres <- API.getToken dzTokenUrl (TokenReq dzClientId dzClientSecret)
      case eres of
        Left err -> throwJsonError500 "TOKEN_ERR" (show err)
        Right (TokenRes token) -> do
          Dz.insertToken dzClientId token
          return token
    Just token -> return token

returnAck :: Context -> Flow AckResponse
returnAck context = return $ AckResponse context (ack "ACK") Nothing

validateReturn :: Order -> Flow ()
validateReturn currOrder =
  when (currOrder ^. #_type == Just "RETURN") $ do
    prevOrderId <- currOrder ^. #_prev_order_id & fromMaybeM400 "INVALID_ORDER_ID"
    prevOrderCase <- Storage.findById (CaseId prevOrderId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
    (prevOrderDetails :: OrderDetails) <- prevOrderCase ^. #_udf1 >>= decodeFromText & fromMaybeM400 "ORDER_NOT_FOUND"
    let prevOrder = prevOrderDetails ^. #order
    -- validating that the items which are returned should be a subset of items in the actual order.
    -- would fail when there are duplicates in current order items
    unless (null $ (Item._id <$> currOrder ^. #_items) List.\\ (Item._id <$> prevOrder ^. #_items)) $
      throwJsonError400 "ERR" "INVALID_RETURN_ORDER"
