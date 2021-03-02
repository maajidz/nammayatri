{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.Confirm (confirm) where

import App.Types
import Beckn.Types.App
import Beckn.Types.ID
import qualified Beckn.Types.Core.API.Callback as API
import qualified Beckn.Types.Core.API.Confirm as API
import qualified Beckn.Types.Core.Ack as Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Domain as Domain
import qualified Beckn.Types.Core.Error as Error
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Common
import qualified Data.Text as T
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import External.Gateway.Transform as GT
import qualified Models.Case as Case
import qualified Product.BecknProvider.BP as BP
import Product.Person (calculateDriverPool, setDriverPool)
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Test.RandomStrings as RS
import qualified Types.Storage.RideRequest as RideRequest
import Utils.Common

confirm :: OrganizationId -> Organization.Organization -> API.ConfirmReq -> FlowHandler Ack.AckResponse
confirm transporterId bapOrg req = withFlowHandler $ do
  logInfo "confirm API Flow" "Reached"
  BP.validateContext "confirm" $ req ^. #context
  let prodInstId = ID $ req ^. #message . #order . #_id
  productInstance <- ProductInstance.findById prodInstId
  let transporterId' = OrganizationId $ productInstance ^. #_organizationId
  transporterOrg <- Organization.findOrganizationById transporterId'
  unless (transporterId' == transporterId) (throwError400 "DIFFERENT_TRANSPORTER_IDS")
  let caseShortId = _getOrganizationId transporterId <> "_" <> req ^. #context . #_transaction_id
  searchCase <- Case.findBySid caseShortId
  bapOrgId <- searchCase ^. #_udf4 & fromMaybeM500 "BAP_ORG_NOT_SET"
  unless (bapOrg ^. #_id == OrganizationId bapOrgId) (throwError400 "BAP mismatch")
  orderCase <- mkOrderCase searchCase
  _ <- Case.create orderCase
  orderProductInstance <- mkOrderProductInstance (orderCase ^. #_id) productInstance
  ProductInstance.create orderProductInstance
  RideRequest.create
    =<< BP.mkRideReq (orderProductInstance ^. #_id) RideRequest.ALLOCATION
  _ <- Case.updateStatus (orderCase ^. #_id) Case.INPROGRESS
  _ <- ProductInstance.updateStatus (productInstance ^. #_id) ProductInstance.CONFIRMED
  --TODO: need to update other product status to VOID for this case
  (currTime, uuid, shortId) <- BP.getIdShortIdAndTime
  let trackerCase = mkTrackerCase searchCase uuid currTime shortId
  _ <- Case.create trackerCase
  uuid1 <- L.generateGUID
  trackerProductInstance <- mkTrackerProductInstance uuid1 (trackerCase ^. #_id) productInstance currTime
  ProductInstance.create trackerProductInstance
  _ <- Case.updateStatus (searchCase ^. #_id) Case.COMPLETED

  fork "OnConfirmRequest" $ onConfirmCallback bapOrg orderProductInstance productInstance orderCase searchCase trackerCase transporterOrg
  mkAckResponse uuid "confirm"

onConfirmCallback ::
  Organization.Organization ->
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstance ->
  Case.Case ->
  Case.Case ->
  Case.Case ->
  Organization.Organization ->
  Flow ()
onConfirmCallback bapOrg orderProductInstance productInstance orderCase searchCase trackerCase transporterOrg = do
  let transporterId = transporterOrg ^. #_id
  let prodInstId = productInstance ^. #_id
  result <- runSafeFlow $ do
    pickupPoint <- (productInstance ^. #_fromLocation) & fromMaybeM500 "NO_FROM_LOCATION"
    vehicleVariant :: Vehicle.Variant <-
      (orderCase ^. #_udf1 >>= readMaybe . T.unpack)
        & fromMaybeM500 "NO_VEHICLE_VARIANT"
    driverPool <- calculateDriverPool (LocationId pickupPoint) transporterId vehicleVariant
    setDriverPool prodInstId driverPool
    logInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| getId prodInstId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
  let bppShortId = _getShortOrganizationId $ transporterOrg ^. #_shortId
  case result of
    Right () -> notifySuccessGateway callbackUrl bppShortId
    Left err -> do
      logError "OnConfirmCallback" $ "Error happened when sending on_confirm request. Error: " +|| err ||+ ""
      notifyErrorGateway err callbackUrl bppShortId
  where
    notifySuccessGateway callbackUrl bppShortId = do
      allPis <- ProductInstance.findAllByCaseId (searchCase ^. #_id)
      onConfirmPayload <- mkOnConfirmPayload searchCase [orderProductInstance] allPis trackerCase
      logInfo "OnConfirmCallback" $ "Sending OnConfirm payload to " +|| callbackUrl ||+ " with payload " +|| onConfirmPayload ||+ ""
      _ <- Gateway.onConfirm callbackUrl onConfirmPayload bppShortId
      pure ()
    notifyErrorGateway err callbackUrl bppShortId = do
      currTime <- getCurrTime
      appEnv <- ask
      let context =
            Context.Context
              { _domain = Domain.MOBILITY,
                _country = Just "IND",
                _city = Nothing,
                _action = "on_confirm",
                _core_version = Just "0.8.2",
                _domain_version = Just "0.8.2",
                _transaction_id = last $ T.split (== '_') $ searchCase ^. #_shortId,
                _message_id = searchCase ^. #_shortId,
                _bap_uri = Nothing,
                _bpp_uri = Just $ BP.makeBppUrl transporterOrg $ nwAddress appEnv,
                _timestamp = currTime,
                _ttl = Nothing
              }
      let payload =
            API.CallbackReq
              { context,
                contents =
                  Left $
                    Error.Error
                      { _type = "DOMAIN-ERROR",
                        _code = err,
                        _path = Nothing,
                        _message = Nothing
                      }
              }
      _ <- Gateway.onConfirm callbackUrl payload bppShortId
      pure ()

mkOrderCase :: Case.Case -> Flow Case.Case
mkOrderCase Case.Case {..} = do
  (now, cid, shortId) <- BP.getIdShortIdAndTime
  return $
    Case.Case
      { _id = cid,
        _name = Nothing,
        _description = Just "Case to order a Ride",
        _shortId = shortId,
        _industry = Case.MOBILITY,
        _type = Case.RIDEORDER,
        _parentCaseId = Just _id,
        _status = Case.INPROGRESS,
        _fromLocationId = _fromLocationId,
        _toLocationId = _toLocationId,
        _createdAt = now,
        _updatedAt = now,
        ..
      }

mkOrderProductInstance :: ID Case.Case -> ProductInstance.ProductInstance -> Flow ProductInstance.ProductInstance
mkOrderProductInstance caseId prodInst = do
  (now, pid, shortId) <- BP.getIdShortIdAndTime
  inAppOtpCode <- generateOTPCode
  return $
    ProductInstance.ProductInstance
      { _id = ID pid,
        _caseId = caseId,
        _productId = prodInst ^. #_productId,
        _personId = Nothing,
        _personUpdatedAt = Nothing,
        _entityType = ProductInstance.VEHICLE,
        _entityId = Nothing,
        _shortId = shortId,
        _quantity = 1,
        _price = prodInst ^. #_price,
        _type = Case.RIDEORDER,
        _organizationId = prodInst ^. #_organizationId,
        _fromLocation = prodInst ^. #_fromLocation,
        _toLocation = prodInst ^. #_toLocation,
        _startTime = prodInst ^. #_startTime,
        _endTime = prodInst ^. #_endTime,
        _validTill = prodInst ^. #_validTill,
        _parentId = Just (prodInst ^. #_id),
        _status = ProductInstance.CONFIRMED,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now,
        _udf1 = prodInst ^. #_udf1,
        _udf2 = prodInst ^. #_udf2,
        _udf3 = prodInst ^. #_udf3,
        _udf4 = Just inAppOtpCode,
        _udf5 = prodInst ^. #_udf5
      }

mkTrackerCase :: Case.Case -> Text -> UTCTime -> Text -> Case.Case
mkTrackerCase case_@Case.Case {..} uuid now shortId =
  Case.Case
    { _id = ID uuid,
      _name = Nothing,
      _description = Just "Case to track a Ride",
      _shortId = shortId,
      _industry = Case.MOBILITY,
      _type = Case.LOCATIONTRACKER,
      _status = Case.NEW,
      _parentCaseId = Just $ case_ ^. #_id,
      _fromLocationId = case_ ^. #_fromLocationId,
      _toLocationId = case_ ^. #_toLocationId,
      _createdAt = now,
      _updatedAt = now,
      ..
    }

mkTrackerProductInstance :: Text -> ID Case.Case -> ProductInstance.ProductInstance -> UTCTime -> Flow ProductInstance.ProductInstance
mkTrackerProductInstance piId caseId prodInst currTime = do
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return $
    ProductInstance.ProductInstance
      { _id = ID piId,
        _caseId = caseId,
        _productId = prodInst ^. #_productId,
        _personId = Nothing,
        _personUpdatedAt = Nothing,
        _shortId = shortId,
        _entityType = ProductInstance.VEHICLE,
        _parentId = Just (prodInst ^. #_id),
        _organizationId = prodInst ^. #_organizationId,
        _entityId = Nothing,
        _type = Case.LOCATIONTRACKER,
        _startTime = prodInst ^. #_startTime,
        _endTime = prodInst ^. #_endTime,
        _fromLocation = prodInst ^. #_fromLocation,
        _toLocation = prodInst ^. #_toLocation,
        _validTill = prodInst ^. #_validTill,
        _quantity = 1,
        _price = 0,
        _status = ProductInstance.INSTOCK,
        _info = Nothing,
        _createdAt = currTime,
        _updatedAt = currTime,
        _udf1 = prodInst ^. #_udf1,
        _udf2 = prodInst ^. #_udf2,
        _udf3 = prodInst ^. #_udf3,
        _udf4 = prodInst ^. #_udf4,
        _udf5 = prodInst ^. #_udf5
      }

mkOnConfirmPayload :: Case.Case -> [ProductInstance.ProductInstance] -> [ProductInstance.ProductInstance] -> Case.Case -> Flow API.OnConfirmReq
mkOnConfirmPayload c pis _allPis trackerCase = do
  context <- BP.mkContext "on_confirm" $ c ^. #_shortId -- TODO : What should be the txnId
  trip <- BP.mkTrip trackerCase (head pis)
  order <- GT.mkOrder c (head pis) (Just trip)
  return
    API.CallbackReq
      { context,
        contents = Right $ API.ConfirmOrder order
      }
